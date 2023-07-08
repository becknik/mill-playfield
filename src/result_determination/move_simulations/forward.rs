//! To containerize all possible in-place functions with a backup to be applied after the operations executed.
//! We call them "simulations"

use smallvec::SmallVec;

use crate::{result_determination::TO_TAKE_VEC_SIZE, DirectionToCheck, EfficientPlayField, FieldPos, MoveDirection};

use super::MOVES_VEC_SIZE;

impl EfficientPlayField {
    /// Simulates the move implicitly specified in the function header by it's parameters, but also all implications
    /// like the takes of moves which end up in a closed mill.
    pub(crate) fn simulate_possible_forward_moves_for(
        &mut self,
        fields_to_take: &SmallVec<[FieldPos; TO_TAKE_VEC_SIZE]>,
        start: FieldPos,
        direction: MoveDirection,
        color: u16,
        simulated_playfields: &mut SmallVec<[EfficientPlayField; MOVES_VEC_SIZE]>,
    ) {
        let start_ring_backup = self.state[start.ring_index];

        // Clear out the start index
        self.state[start.ring_index] &= !(3u16 << (start.index * 2));

        if let MoveDirection::AcrossRings { target_ring_index } = direction {
            let target_ring_backup = self.state[target_ring_index];

            // Moving a stone onto the field on the other ring
            self.state[target_ring_index] |= color << (start.index * 2);

            let target_pos = FieldPos { ring_index: target_ring_index, ..start };
            self.add_simulated_moves(target_pos, direction, color, simulated_playfields, fields_to_take);

            self.state[target_ring_index] = target_ring_backup;
        } else if let MoveDirection::OnRing { target_field_index } = direction {
            // Set the empty neighbors value to the old one of the current index:
            self.state[start.ring_index] |= color << (target_field_index * 2);

            let target_pos = FieldPos { index: target_field_index, ..start };
            self.add_simulated_moves(target_pos, direction, color, simulated_playfields, fields_to_take);
        }
        // End simulation by applying backup state
        self.state[start.ring_index] = start_ring_backup;
    }

    /// Check for mills on the simulated move which is done in-place in self.
    ///
    /// If there is a new mill due to the simulated move, take the free-to-take positions stones contained in the `fields_to_take`
    /// vector by calling the `add_simulated_takes` function
    ///
    /// TODO Silly name
    fn add_simulated_moves(
        &mut self,
        start: FieldPos,
        direction: MoveDirection,
        color: u16,
        simulated_playfields: &mut SmallVec<[EfficientPlayField; MOVES_VEC_SIZE]>,
        fields_to_take: &SmallVec<[FieldPos; TO_TAKE_VEC_SIZE]>,
    ) {
        let possible_mills_count = match direction {
            MoveDirection::OnRing { target_field_index } => self.get_mill_count(
                FieldPos { index: target_field_index, ..start },
                DirectionToCheck::OnAndAcrossRings { player_color: color },
            ),
            MoveDirection::AcrossRings { target_ring_index } => {
                self.get_mill_count(FieldPos { ring_index: target_ring_index, ..start }, DirectionToCheck::OnRing)
            }
        };

        if 0 < possible_mills_count {
            self.add_simulated_takes(simulated_playfields, fields_to_take);
        } else {
            simulated_playfields.push(*self);
        }
    }

    /// Simulates the moves taking on element from the `fields_to_take` set & adds them to the simulated_playfields vector
    fn add_simulated_takes(
        &mut self,
        simulated_playfields: &mut SmallVec<[EfficientPlayField; MOVES_VEC_SIZE]>,
        fields_to_take: &SmallVec<[FieldPos; TO_TAKE_VEC_SIZE]>,
    ) {
        let state_backup = self.state;

        for field in fields_to_take {
            self.state[field.ring_index] &= !(3u16 << (field.index * 2));

            simulated_playfields.push(*self);
            self.state = state_backup;
        }
    }
}
