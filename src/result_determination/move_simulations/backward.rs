use smallvec::SmallVec;

use crate::{DirectionToCheck, EfficientPlayField, FieldPos, MoveDirection, PlayerColor};

use super::MOVES_VEC_SIZE;

impl EfficientPlayField {
    // return vec of one move from one given stone
    pub(crate) fn simulate_backward_move_get_playfields(
        &mut self,
        start: FieldPos,
        direction: MoveDirection,
        player_color: PlayerColor,
        simulated_playfields: &mut SmallVec<[EfficientPlayField; MOVES_VEC_SIZE]>,
    ) {
        let stone_color_rep: u16 = player_color.into();

        let start_ring_backup = self.state[start.ring_index];

        let init_mill_count =
            self.get_mill_count(start, DirectionToCheck::OnAndAcrossRings { player_color: stone_color_rep });

        // Clear out the current index
        self.state[start.ring_index] &= !(3u16 << (start.index * 2));

        if let MoveDirection::AcrossRings { target_ring_index } = direction {
            let target_ring_backup = self.state[target_ring_index];

            // Setting the moved stone on the other ring
            self.state[target_ring_index] |= stone_color_rep << (start.index * 2);

            if init_mill_count == 0 {
                simulated_playfields.push(*self);
            } else {
                self.add_simulated_placements(start, player_color, simulated_playfields);
            }

            // Resetting the in-place simulation on the other ring
            self.state[target_ring_index] = target_ring_backup;
        } else if let MoveDirection::OnRing { target_field_index } = direction {
            // Set the empty neighbors value to the old one of the current index:
            self.state[start.ring_index] |= stone_color_rep << (target_field_index * 2);

            if init_mill_count == 0 {
                simulated_playfields.push(*self);
            } else {
                self.add_simulated_placements(start, player_color, simulated_playfields);
            }
        }

        // Resetting the in-place simulation
        self.state[start.ring_index] = start_ring_backup;
    }
}
