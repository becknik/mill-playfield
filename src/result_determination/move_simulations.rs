use smallvec::SmallVec;

use crate::{DirectionToCheck, EfficientPlayField, FieldPos, MoveDirection, PlayerColor};

mod backward;
mod forward;

// simulated_playfield_buffer.len()'s max value after running seems to be 58
const MOVES_VEC_SIZE: usize = 128;
// 5vs5 yielded max of 17 for the buffer, the output was <128 in the middle, but spiked up to 1026...

impl EfficientPlayField {
    /// Returns the set of forawrd moves which can be made by `player_color` with the current playfield configuration
    pub fn get_forward_moves(&mut self, player_color: PlayerColor) -> SmallVec<[EfficientPlayField; MOVES_VEC_SIZE]> {
        // Place here because it seems to be used in both branches...
        let fields_to_take = self.get_fields_to_take_by(player_color);

        let mut forward_moved_playfields = SmallVec::<[EfficientPlayField; MOVES_VEC_SIZE]>::new();
        let mut simulated_playfield_buffer = SmallVec::<[EfficientPlayField; MOVES_VEC_SIZE]>::new();

        for ring_index in 0..3 {
            for field_index in 0..8 {
                let current_field = FieldPos { ring_index, index: field_index };

                let current_field_state = self.get_field_state_at(current_field);

                if current_field_state != (<PlayerColor as Into<u16>>::into(player_color) << (field_index * 2)) {
                    continue;
                }

                let amount_of_stones = self.get_stone_count_of(player_color);

                // only 3 stones of current color? -> enable jumping
                if amount_of_stones == 3 {
                    let backup_state = self.state;

                    //clear current field
                    self.state[ring_index] &= !(3u16 << (field_index * 2));

                    for empty_field in self.get_empty_fields() {
                        if empty_field == (current_field) {
                            continue;
                        }

                        let mut clone = *self;

                        clone.state[empty_field.ring_index] |=
                            <PlayerColor as Into<u16>>::into(player_color) << (empty_field.index * 2);

                        let mills_possible = clone.get_mill_count(
                            empty_field,
                            DirectionToCheck::OnAndAcrossRings { player_color: player_color.into() },
                        );

                        // If no mill occurred, just add the new config
                        if 0 == mills_possible {
                            forward_moved_playfields.push(clone);
                        }
                        // If a new mill occurs through jump, simulate the possible takes &
                        // add them to the forward_moved_playfields vec
                        else {
                            let backup_after_first_move = clone.state;

                            for field in &fields_to_take {
                                clone.state[field.ring_index] &= !(3u16 << (field.index * 2));
                                forward_moved_playfields.push(clone);

                                clone.state = backup_after_first_move;
                            }
                        }
                    }
                    self.state = backup_state;
                } else {
                    for (neighbor_index, neighbor_state) in self.get_neighbor_field_states(current_field) {
                        if neighbor_state == 0 {
                            self.simulate_possible_forward_moves_for(
                                &fields_to_take,
                                current_field,
                                MoveDirection::OnRing { target_field_index: neighbor_index },
                                player_color.into(),
                                &mut simulated_playfield_buffer,
                            );

                            forward_moved_playfields.append(&mut simulated_playfield_buffer);
                        }
                    }

                    if field_index % 2 == 0 {
                        let (next_rings_field_state, previous_rings_field_state) =
                            self.get_neigbor_rings_field_states(current_field);

                        match ring_index {
                            0 if next_rings_field_state == 0 => {
                                self.simulate_possible_forward_moves_for(
                                    &fields_to_take,
                                    FieldPos { ring_index: 0, index: field_index },
                                    MoveDirection::AcrossRings { target_ring_index: 1 },
                                    player_color.into(),
                                    &mut simulated_playfield_buffer,
                                );
                                forward_moved_playfields.append(&mut simulated_playfield_buffer);
                            }
                            1 => {
                                if previous_rings_field_state == 0 {
                                    self.simulate_possible_forward_moves_for(
                                        &fields_to_take,
                                        FieldPos { ring_index: 1, index: field_index },
                                        MoveDirection::AcrossRings { target_ring_index: 0 },
                                        player_color.into(),
                                        &mut simulated_playfield_buffer,
                                    );
                                    forward_moved_playfields.append(&mut simulated_playfield_buffer);
                                }

                                if next_rings_field_state == 0 {
                                    self.simulate_possible_forward_moves_for(
                                        &fields_to_take,
                                        FieldPos { ring_index: 1, index: field_index },
                                        MoveDirection::AcrossRings { target_ring_index: 2 },
                                        player_color.into(),
                                        &mut simulated_playfield_buffer,
                                    );
                                    forward_moved_playfields.append(&mut simulated_playfield_buffer);
                                }
                            }
                            2 if previous_rings_field_state == 0 => {
                                self.simulate_possible_forward_moves_for(
                                    &fields_to_take,
                                    FieldPos { ring_index: 2, index: field_index },
                                    MoveDirection::AcrossRings { target_ring_index: 1 },
                                    player_color.into(),
                                    &mut simulated_playfield_buffer,
                                );
                                forward_moved_playfields.append(&mut simulated_playfield_buffer);
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
        forward_moved_playfields
    }

    #[rustfmt::skip]
    /// Simulates the backward moves of `player_color` by calling [get_fields_to_place] & retruns it
    pub fn get_backward_moves(&mut self, player_color: PlayerColor, max_stone_count: i32) -> SmallVec<[EfficientPlayField; MOVES_VEC_SIZE]> {
        let empty_fields = self.get_empty_fields();

        let mut output_playfields = SmallVec::<[EfficientPlayField; MOVES_VEC_SIZE]>::new();
        let mut simulated_playfield_buffer = SmallVec::<[EfficientPlayField; MOVES_VEC_SIZE]>::new();

        for ring_index in 0..3 {
            for field_index in 0..8 {
                let current_field = FieldPos {ring_index, index: field_index};

                let current_field_state = self.get_field_state_at(current_field);

                if current_field_state != (<PlayerColor as Into<u16>>::into(player_color) << (field_index * 2)) {
                    continue;
                }

                let amount_of_stones = self.get_stone_count_of(player_color);
                let amount_of_stones_enemy = self.get_stone_count_of(!player_color);

                if amount_of_stones == 3 {
                    // Check for mills before the move has taken place
                    let was_in_mill = self.get_mill_count(
                        current_field,
                        DirectionToCheck::OnAndAcrossRings {
                            player_color: player_color.into(),
                        },
                    );

                    let backup_state = self.state;
                    // clear out the current position
                    self.state[ring_index] &= !(3u16 << (field_index * 2));

                    // make jump-moves onto all free positions
                    for empty_field in &empty_fields {
                        let mut clone = *self;

                        // Apply the jump to the state clone
                        clone.state[empty_field.ring_index] |=
                            <PlayerColor as Into<u16>>::into(player_color) << (empty_field.index * 2);

                        if 0 == was_in_mill {
                            output_playfields.push(clone);
                        }
                        // If the jump was made by a stone which was previously located in a mill,
                        // stones from the other color, which were previously taken by the color with the mill,
                        // have to be added to the field again
                        else if (amount_of_stones_enemy as i32) < max_stone_count {
                            clone.add_simulated_placements(current_field, player_color, &mut output_playfields);
                        } else {
                            output_playfields.push(clone);
                            //clone.add_simulated_placements(FieldPos { ring_index, field_index }, player_color, &mut output_playfields);
                        }
                    }
                    self.state = backup_state;
                } else {
                    for (neighbor_index, neighbor_state) in self.get_neighbor_field_states(current_field) {
                        if neighbor_state == 0 {
                            self.simulate_backward_move_get_playfields(
                                current_field,
                                MoveDirection::OnRing {
                                    target_field_index: neighbor_index,
                                },
                                player_color,
                                &mut simulated_playfield_buffer
                            );
                            output_playfields.append(&mut simulated_playfield_buffer);
                        }
                    }

                    // Check for possible over-ring moves
                    if (field_index % 2) == 0 {
                        let (next_rings_field_state, previous_rings_field_state) = self.get_neigbor_rings_field_states(current_field);

                        match ring_index {
                            // Inner Ring
                            0 if next_rings_field_state == 0 => {
                                self.simulate_backward_move_get_playfields(
                                    FieldPos {ring_index: 0, index: field_index },
                                    MoveDirection::AcrossRings { target_ring_index: 1 },
                                    player_color,
                                    &mut simulated_playfield_buffer,
                                );
                                output_playfields.append(&mut simulated_playfield_buffer);
                            }
                            // Mid Ring
                            1 => {
                                if previous_rings_field_state == 0 {
                                    self.simulate_backward_move_get_playfields(
                                        FieldPos {ring_index: 1, index: field_index },
                                        MoveDirection::AcrossRings { target_ring_index: 0 },
                                        player_color,
                                        &mut simulated_playfield_buffer
                                    );
                                    output_playfields.append(&mut simulated_playfield_buffer);
                                }

                                if next_rings_field_state == 0 {
                                    self.simulate_backward_move_get_playfields(
                                        FieldPos {ring_index: 1, index: field_index },
                                        MoveDirection::AcrossRings { target_ring_index: 2 },
                                        player_color,
                                        &mut simulated_playfield_buffer
                                    );
                                    output_playfields.append(&mut simulated_playfield_buffer);
                                }
                            }
                            // Outer Ring
                            2 if previous_rings_field_state == 0 => {
                                self.simulate_backward_move_get_playfields(
                                    FieldPos {ring_index: 2, index: field_index },
                                    MoveDirection::AcrossRings { target_ring_index: 1 },
                                    player_color,
                                    &mut simulated_playfield_buffer
                                );
                                output_playfields.append(&mut simulated_playfield_buffer);
                            }
                            _ => {}
                        }
                    }
                }
                // empty_fields.push(current_tupel); // TODO wtf?! this makes no sense
            }
        }
        output_playfields
    }

    /// Simulates the placement of a stone which was removed by closing a mill in the previous move
    /// by the player with player_color.
    /// Therefore, the stones placed on the playfield by this method are in the opposite color.
    fn add_simulated_placements(
        &mut self,
        start: FieldPos,
        player_color: PlayerColor,
        simulated_playfields: &mut SmallVec<[EfficientPlayField; MOVES_VEC_SIZE]>,
    ) {
        let backup_after_move = self.state;

        for empty_field in self.get_empty_fields() {
            // sorts out initial mill position
            if start == empty_field {
                continue;
            }

            // adds opposite colored stone (which has been taken be the mill) to empty field
            self.state[empty_field.ring_index] |=
                <PlayerColor as Into<u16>>::into(!player_color) << (empty_field.index * 2);

            // if the placed stone of the opposite color could be taken now,
            // the placement of this stone would be valid and the current playfield config should be pushed
            if self.get_fields_to_take_by(player_color).contains(&empty_field) {
                simulated_playfields.push(*self);
            }

            self.state = backup_after_move;
        }
    }
}
