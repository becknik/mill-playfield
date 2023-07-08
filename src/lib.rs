//! This module holds the representation part of the more efficient variant of the [PlayField] struct with some low-level
//! functions for accessing and modifying it's state & convert it to a canonical form, which is needed in the later parts
//! of the project.
//!
//! It also holds some functions required to calculate the total amount of won & lost playfield configurations for an
//! arbitrary amount of start stones.

use std::{cmp::Ordering, hash::Hash};

pub use smallvec::SmallVec;
use RelativePosition::*;

pub mod de_encoding;
pub mod printing;
pub mod result_determination;

struct EfficientPlayField4 {
    states: [u16; 12],
}

/// Efficient representation of a mill playfield using a [u16; 3] for it's internal representation.
/// Start counting from the top, inner middle field on the LSB of each u16 field for each of the 3 rectangle rings.
/// The inner ring equals the index 0 in the representation array.
///
/// Using three states coded as following:
/// - 00: free
/// - 01: white
/// - 10: black
/// - 11: undefined -> should never be reached
#[derive(Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct EfficientPlayField {
    state: [u16; 3],
}

impl PartialOrd for EfficientPlayField {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        //let state = self.state;
        //let other_state = other.state;

        Option::Some(if self.state < other.state {
            Ordering::Less
        } else if self.state > other.state {
            Ordering::Greater
        } else {
            Ordering::Equal
        })
    }
}

/// A coordinate of the [EfficientPlayField] wich is indexed as following:
/// - The `ring_index` starts counting on the most inner ring of the playfield and therefore should be in 0..3
/// - The `index` starts counting on the top, middle index on the ring and traverses the field clock-wise
/// -> Should be in 0..8 due to the use of "abstract" index representation
#[derive(Clone, Copy, PartialEq, Hash, Eq)]
pub struct FieldPos {
    pub ring_index: usize,
    pub index: u32,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum FieldState {
    Free = 0b11,
    White = 0b10,
    Black = 0b01,
}

#[derive(Debug, Clone, Copy)]
pub enum PlayerColor {
    White,
    Black,
}

impl std::fmt::Display for PlayerColor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PlayerColor::White => f.write_str("●"),
            PlayerColor::Black => f.write_str("○"),
        }
    }
}

impl Into<FieldState> for PlayerColor {
    fn into(self) -> FieldState {
        match self {
            PlayerColor::White => FieldState::White,
            PlayerColor::Black => FieldState::Black,
        }
    }
}

impl Into<u16> for PlayerColor {
    /// Needed for the [EfficientPlayField] representation of the enum
    fn into(self) -> u16 {
        match self {
            PlayerColor::White => 1u16,
            PlayerColor::Black => 2u16,
        }
    }
}

impl std::ops::Not for PlayerColor {
    type Output = PlayerColor;

    fn not(self) -> Self::Output {
        if let PlayerColor::White = self {
            PlayerColor::Black
        } else {
            PlayerColor::White
        }
    }
}

/// Used by the [get_ring_index] method of [EfficientPlayField]
pub enum RelativePosition {
    Previous,
    Next,
}

/// Used by the [simulate_move_then_get_mills] method of [EfficientPlayField]
pub enum MoveDirection {
    OnRing { target_field_index: u32 },
    AcrossRings { target_ring_index: usize },
}

/// Used by the [get_mill_count] method of [EfficientPlayField]
pub enum DirectionToCheck {
    OnRing,
    OnAndAcrossRings { player_color: u16 },
}

impl EfficientPlayField {
    /// Sets the field according to the binary parameters. The indices are specified binary coded
    ///
    /// Handled extreme cases:
    /// - Ensures that black or white fields are replaced by free, vice versa
    /// - The input parameters must have values that make sense, ring_index < 3, index < 8, state < 3
    pub fn set_field_state(&mut self, field: FieldPos, field_state: FieldState) {
        assert!(field.ring_index < 3usize, "Ring index is larger than 0x03");

        assert!(field.index < 8u32, "Index is greater than 0x07");

        let old_ring_state = self.state[field.ring_index];

        // Assert target field is free, when field_state to is not
        if !matches!(field_state, FieldState::Free) {
            assert!((old_ring_state & (3u16 << (field.index * 2))) == 0, "Tried to place non-free on non-free");
        // Assert target field is not free, when field_state is
        } else {
            assert!((old_ring_state & (3u16 << (field.index * 2))) != 0, "Tried to place free on free");
        }

        // Shifting mask upon field index & applying it with disjunction
        let new_state_mask = (field_state as u16) << (field.index * 2);
        self.state[field.ring_index] = old_ring_state | new_state_mask;
    }

    /// Bitmasks out the specified field and returns it either aligned to the LSB or unaligned as masked out.
    ///
    /// The field_index must be < 8!
    pub fn get_field_state_at(&self, field: FieldPos) -> u16 {
        //assert!(field_index < 8, "`get_field_state_at` makes use of an abstract index representation");
        return self.state[field.ring_index] & (3u16 << (field.index * 2));
    }

    pub fn get_ring_index(ring_index: usize, relative: RelativePosition) -> usize {
        return (ring_index
            + match relative {
                RelativePosition::Previous => 2,
                RelativePosition::Next => 1,
            })
            % 3;
    }

    /// Returns the (index in abstract form, state) of the on-ring neighbor of the specified field.
    /// No alignment to the LSB is done.
    pub fn get_neighbor_field_states(&self, field: FieldPos) -> [(u32, u16); 2] {
        let indices = [(field.index + 7) % 8, (field.index + 1) % 8];

        return [
            (indices[0], self.get_field_state_at(FieldPos { index: indices[0], ..field })),
            (indices[1], self.get_field_state_at(FieldPos { index: indices[1], ..field })),
        ];
    }

    /// Rotates the rings of the mill in the right direction
    fn rotate_self_right(&mut self, amount: u32) {
        for ring_index in 0..3 {
            // Due to rep staring on the LSB, we have to shift left
            // 2 fields to shift, which equals 4 bits in total
            self.state[ring_index] = self.state[ring_index].rotate_left(2 * 2 * amount);
        }
    }

    /// Swaps the inner ring/ rect in place with the outer ring
    fn swap_rings(&mut self) {
        self.state.swap(0, 2);
    }

    /// In-situ mirroring on an imaginary y-axis
    fn mirror_on_y(&mut self) {
        for ring_index in 0..3 {
            let current_ring = self.state[ring_index];

            let left_side = ((current_ring & 0b0000_0000_1100_0000u16) >> 4)
                | ((current_ring & 0b0000_0000_0000_1100u16) << 4)
                | (0b0000_0000_0011_0000u16 & current_ring);

            let right_side = ((current_ring & 0b1100_0000_0000_0000u16) >> 4)
                | ((current_ring & 0b0000_1100_0000_0000u16) << 4)
                | (0b0011_0000_0000_0000u16 & current_ring);

            self.state[ring_index] = (current_ring & 0b0000_0011_0000_0011u16) | (left_side << 8) | (right_side >> 8);
        }
    }

    /// The canonical form of EfficientPlayField ist created by selecting the length-lexicographical largest representant
    /// of the EfficientPlayField elements in the equivalent class.
    /// This is done by the application of mirroring on the y-axis, rotations & swaps of the inner and outer ring.
    pub fn get_canon_form(&mut self) -> EfficientPlayField {
        let mut canonical_form = *self;

        let mut self_mirrored = *self;
        self_mirrored.mirror_on_y();

        let mut self_swapped = *self;
        self_swapped.swap_rings();
        let mut self_swapped_mirrored = self_mirrored;
        self_swapped_mirrored.swap_rings();

        // Loop unrolling it is due to code being hot
        /* for _i in 0..2 {
            for _j in 0..4 {

                if self > &mut canonical_form {
                    canonical_form = *self
                }

                if mirrored_self > canonical_form {
                    canonical_form = mirrored_self
                }

                mirrored_self.rotate_self_right(1);
                self.rotate_self_right(1);
            }
            mirrored_self.swap_rings();
            self.swap_rings();
        } */

        // 1
        /* if self > &mut canonical_form {
            canonical_form = *self
        } */
        if self_mirrored > canonical_form {
            canonical_form = self_mirrored
        }
        self_mirrored.rotate_self_right(1);
        self.rotate_self_right(1);

        // 2
        if self > &mut canonical_form {
            canonical_form = *self
        }
        if self_mirrored > canonical_form {
            canonical_form = self_mirrored
        }
        self_mirrored.rotate_self_right(1);
        self.rotate_self_right(1);

        // 3
        if self > &mut canonical_form {
            canonical_form = *self
        }
        if self_mirrored > canonical_form {
            canonical_form = self_mirrored
        }
        self_mirrored.rotate_self_right(1);
        self.rotate_self_right(1);

        // 4
        if self > &mut canonical_form {
            canonical_form = *self
        }
        if self_mirrored > canonical_form {
            canonical_form = self_mirrored
        }

        // ---------------------------------------------------------------------

        // 1
        if self_swapped > canonical_form {
            canonical_form = self_swapped
        }
        if self_swapped_mirrored > canonical_form {
            canonical_form = self_swapped_mirrored
        }
        self_swapped.rotate_self_right(1);
        self_swapped_mirrored.rotate_self_right(1);

        // 2
        if self_swapped > canonical_form {
            canonical_form = self_swapped
        }
        if self_swapped_mirrored > canonical_form {
            canonical_form = self_swapped_mirrored
        }
        self_swapped.rotate_self_right(1);
        self_swapped_mirrored.rotate_self_right(1);

        // 3
        if self_swapped > canonical_form {
            canonical_form = self_swapped
        }
        if self_swapped_mirrored > canonical_form {
            canonical_form = self_swapped_mirrored
        }
        self_swapped.rotate_self_right(1);
        self_swapped_mirrored.rotate_self_right(1);

        // 4
        if self_swapped > canonical_form {
            canonical_form = self_swapped
        }
        if self_swapped_mirrored > canonical_form {
            canonical_form = self_swapped_mirrored
        }

        canonical_form
    }

    /// Traverses all positions on the playfield to collect the field positions of color & !color into the Vecs to be
    /// returned, respectively
    pub fn get_positions_of_stones(&self, color: PlayerColor) -> (SmallVec<[FieldPos; 9]>, SmallVec<[FieldPos; 9]>) {
        let mut color_positions = SmallVec::<[FieldPos; 9]>::default();
        let mut not_color_positions = SmallVec::<[FieldPos; 9]>::default();

        for ring_index in 0..3 {
            for field_index in 0..8 {
                let state = self.get_field_state_at(FieldPos { ring_index, index: field_index });

                if state == (<PlayerColor as Into<u16>>::into(color) << (field_index * 2)) {
                    color_positions.push(FieldPos { ring_index, index: field_index })
                } else if state == (<PlayerColor as Into<u16>>::into(!color) << (field_index * 2)) {
                    not_color_positions.push(FieldPos { ring_index, index: field_index })
                }
            }
        }
        (color_positions, not_color_positions)
    }

    /// Simulates a move of the stones of the start fields and ring index to either a it's neighboring target index or
    /// the start index on another ring, which is determined by the [MoveDirection] enum.
    ///
    /// Preconditions:
    /// - Indices should already be in "abstract form" x < 8
    /// - The target field/ the start index on the other ring must be empty
    pub fn simulate_move_get_mill_count(&mut self, start: FieldPos, direction: MoveDirection, color: u16) -> u32 {
        assert!(start.index < 8);
        assert!(start.ring_index < 3);

        // To rollback the in-situ changes on self
        let start_ring_backup = self.state[start.ring_index];

        // Clear out the current index, must be done when simulating the moving in general
        self.state[start.ring_index] &= !(3u16 << (start.index * 2));

        let mills_possible = if let MoveDirection::AcrossRings { target_ring_index } = direction {
            assert!(target_ring_index < 3);

            // To rollback the second in-situ changes on self
            let target_ring_backup = self.state[target_ring_index];

            // Setting the state of the other index, which must be empty
            self.state[target_ring_index] |= color << (start.index * 2);

            // TODO makes this sense to you, future me? :|
            let mills_possible =
                self.get_mill_count(FieldPos { ring_index: target_ring_index, ..start }, DirectionToCheck::OnRing);
            //let mills_possible = self.get_mill_count(target_ring_index, start_fields_index, DirectionToCheck::OnAndAcrossRings { player_color: color });

            // Resetting the in-place simulation on the other ring
            self.state[target_ring_index] = target_ring_backup;

            mills_possible
        } else if let MoveDirection::OnRing { target_field_index } = direction {
            assert!(target_field_index < 8);

            // Set the empty neighbors value to the old one of the current index:
            self.state[start.ring_index] |= color << (target_field_index * 2);

            // Check for mills after the move now has taken place
            self.get_mill_count(
                FieldPos { index: target_field_index, ..start },
                DirectionToCheck::OnAndAcrossRings { player_color: color },
            )
        } else {
            0
        };

        // Resetting the in-place simulation
        self.state[start.ring_index] = start_ring_backup;
        mills_possible
    }

    /// Checks for mills on the specified field & returns it.
    /// The check for mills across rings are toggled when the right argument is set. The tuple enum is there to avoid
    /// the re-calculation of the field state of the current index which should be determined on call-time
    pub fn get_mill_count(&self, field: FieldPos, direction: DirectionToCheck) -> u32 {
        assert!(field.index < 8);
        assert!(field.ring_index < 3);

        let mut mill_counter = 0;

        /* Rotations of the real play field anti-clockwise per index for alignment on the index 0:
        0,1 => 7
        1 => 1
        2,3 => 1
        3 => 3
        4,5 => 3
        5 => 5
        6,7 => 5
        7 => 7
        */
        let rep_indices_to_rotate = (((field.index - (field.index % 2) + 7) % 8) * 2) as u32;
        // Field state triple containing field_index:
        let state_triple = self.state[field.ring_index].rotate_right(rep_indices_to_rotate) & 0b0000_0000_0011_1111u16;

        /* 010101 | 101010 */
        if state_triple == 21u16 || state_triple == 42u16 {
            mill_counter += 1;
        }

        // If index is located in an edge, two triples must be checked for mill occurrence
        if field.index % 2 == 1 {
            let state_triple = self.state[field.ring_index].rotate_right(field.index * 2) & 0b0000_0000_0011_1111u16;
            /* 010101 | 101010 */
            if state_triple == 21u16 || state_triple == 42u16 {
                mill_counter += 1;
            }
        }

        // Argument field index in the middle of a triple and therefore can form a mill connected to the other rings
        if let DirectionToCheck::OnAndAcrossRings { player_color } = direction {
            if field.index % 2 == 0 {
                let next_rindex_field_state = self
                    .get_field_state_at(FieldPos { ring_index: Self::get_ring_index(field.ring_index, Next), ..field });
                let previous_rindex_field_state = self.get_field_state_at(FieldPos {
                    ring_index: Self::get_ring_index(field.ring_index, Previous),
                    ..field
                });

                // Mill in between rings:
                if next_rindex_field_state == (player_color << (field.index * 2))
                    && next_rindex_field_state == previous_rindex_field_state
                {
                    mill_counter += 1;
                }
            }
        }
        mill_counter
    }
}

#[cfg(test)]
mod tests {
    use super::EfficientPlayField;

    mod normal {
        use crate::{FieldPos, FieldState};

        use super::*;

        #[test]
        fn set_field_normal() {
            let mut epf = EfficientPlayField::default();

            epf.set_field_state(FieldPos { ring_index: 2, index: 7 }, FieldState::Black);
            epf.set_field_state(FieldPos { ring_index: 1, index: 7 }, FieldState::White);
            epf.set_field_state(FieldPos { ring_index: 0, index: 7 }, FieldState::Free);

            epf.set_field_state(FieldPos { ring_index: 1, index: 0 }, FieldState::White);
            epf.set_field_state(FieldPos { ring_index: 1, index: 1 }, FieldState::White);
            epf.set_field_state(FieldPos { ring_index: 2, index: 2 }, FieldState::White);
            epf.set_field_state(FieldPos { ring_index: 2, index: 3 }, FieldState::White);
            epf.set_field_state(FieldPos { ring_index: 2, index: 4 }, FieldState::White);
            epf.set_field_state(FieldPos { ring_index: 2, index: 5 }, FieldState::White);
            epf.set_field_state(FieldPos { ring_index: 2, index: 6 }, FieldState::White);

            epf.set_field_state(FieldPos { ring_index: 2, index: 6 }, FieldState::Free);
            epf.set_field_state(FieldPos { ring_index: 2, index: 2 }, FieldState::Black);
            epf.set_field_state(FieldPos { ring_index: 1, index: 4 }, FieldState::Black);

            println!("\nAfter some added stones: {}", epf);
        }

        #[test]
        fn rotate1() {
            let mut epf = EfficientPlayField::default();

            epf.set_field_state(FieldPos { ring_index: 2, index: 0 }, FieldState::White);
            epf.set_field_state(FieldPos { ring_index: 1, index: 1 }, FieldState::Black);
            epf.set_field_state(FieldPos { ring_index: 0, index: 2 }, FieldState::White);

            println!("\nInitial state: {}", epf);
            epf.rotate_self_right(1);
            println!("First rotation: {}", epf);
            epf.rotate_self_right(1);
            println!("Second rotation: {}", epf);
            epf.rotate_self_right(1);
            println!("Third rotation: {}", epf);

            /* assert!(epf.state[2] == 0x0004);
            assert!(epf.state[1] == 0x0010);
            assert!(epf.state[1] == 0x0010); */

            epf.rotate_self_right(2);

            /* assert!(epf.state[2] == 0x0010);
            assert!(epf.state[1] == 0x0080);
            assert!(epf.state[1] == 0x0100); */

            epf.rotate_self_right(3);
        }

        #[test]
        fn mirror1() {
            let mut epf = EfficientPlayField::default();

            epf.set_field_state(FieldPos { ring_index: 2, index: 0 }, FieldState::White);
            epf.set_field_state(FieldPos { ring_index: 1, index: 1 }, FieldState::Black);
            epf.set_field_state(FieldPos { ring_index: 0, index: 2 }, FieldState::White);
            epf.set_field_state(FieldPos { ring_index: 1, index: 3 }, FieldState::Black);

            println!("\nNot mirrored:{}", epf);

            epf.mirror_on_y();

            println!("Mirrored: {}", epf);
        }

        #[test]
        fn canonical1() {
            let test = "BBEEEEEBEEEEWEWWBWWEEEBE";
            println!("Input: {test}");

            let mut epf = EfficientPlayField::from_coded(test);

            println!("{}", epf);

            let epf = epf.get_canon_form();

            println!("Output: {}", epf.to_string_representation());
        }
    }

    mod extreme {
        use crate::{FieldPos, FieldState};

        use super::*;

        #[test]
        #[should_panic]
        fn set_field_black_to_white() {
            let mut epf = EfficientPlayField::default();

            epf.set_field_state(FieldPos { ring_index: 2, index: 7 }, FieldState::Black);
            epf.set_field_state(FieldPos { ring_index: 2, index: 7 }, FieldState::Black);
        }

        #[test]
        #[should_panic]
        fn set_field_white_to_black() {
            let mut epf = EfficientPlayField::default();

            epf.set_field_state(FieldPos { ring_index: 2, index: 7 }, FieldState::Black);
            epf.set_field_state(FieldPos { ring_index: 2, index: 7 }, FieldState::White);
        }

        #[test]
        #[should_panic]
        fn set_field_free_to_free() {
            let mut epf = EfficientPlayField::default();

            epf.set_field_state(FieldPos { ring_index: 3, index: 1 }, FieldState::Free);
        }

        #[test]
        #[should_panic]
        fn set_ring_index_to_11() {
            let mut epf = EfficientPlayField::default();

            epf.set_field_state(FieldPos { ring_index: 2, index: 8 }, FieldState::Black);
        }
    }
}
