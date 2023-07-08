use fnv::FnvHashSet;

use crate::PlayerColor;
use crate::{DirectionToCheck, EfficientPlayField, FieldPos, FieldState, MoveDirection};

impl EfficientPlayField {
    pub fn generate_start_won_configs_white(max_stone_count: i32) -> FnvHashSet<EfficientPlayField> {
        let mut won_set = FnvHashSet::<EfficientPlayField>::default();
        let mill_configs = Self::generate_3_canon_mills();

        for canon_mill in mill_configs {
            for i in 0..24 {
                let current_field = FieldPos { ring_index: (i / 8) as usize, index: i % 8 };

                // to avoid placing stones onto already present mills
                if canon_mill.get_field_state_at(current_field) != 0 {
                    continue;
                }

                let mut config = canon_mill;
                //adding first black stone
                config.state[current_field.ring_index] |= 2u16 << (current_field.index * 2);

                for j in (i + 1)..24 {
                    let current_field = FieldPos { ring_index: (j / 8) as usize, index: j % 8 };

                    if canon_mill.get_field_state_at(current_field) != 0 {
                        continue;
                    }

                    let mut config = config.clone();
                    //adding second black stone
                    config.state[current_field.ring_index] |= 2u16 << (current_field.index * 2);

                    won_set.insert(config.get_canon_form());

                    // white stones must be placed before black ones => start_index = 0
                    config.distribute_stones_and_add(PlayerColor::White, max_stone_count - 3, 0, &mut won_set);
                }
            }
        }

        Self::generate_black_enclosed_configs(max_stone_count, &mut won_set);

        won_set
    }

    /// Hard-coded generation of the only 3 unique mill playfield configuration
    /// Uses the the mirroring & rotation of the play field and swapping of ring index which is done by the canonical form generation
    fn generate_3_canon_mills() -> [EfficientPlayField; 3] {
        let mut pf1 = EfficientPlayField::default();
        pf1.set_field_state(FieldPos { ring_index: 2, index: 7 }, FieldState::White);
        pf1.set_field_state(FieldPos { ring_index: 2, index: 0 }, FieldState::White);
        pf1.set_field_state(FieldPos { ring_index: 2, index: 1 }, FieldState::White);

        let mut pf2 = EfficientPlayField::default();
        pf2.set_field_state(FieldPos { ring_index: 1, index: 7 }, FieldState::White);
        pf2.set_field_state(FieldPos { ring_index: 1, index: 0 }, FieldState::White);
        pf2.set_field_state(FieldPos { ring_index: 1, index: 1 }, FieldState::White);

        let mut pf3 = EfficientPlayField::default();
        pf3.set_field_state(FieldPos { ring_index: 0, index: 2 }, FieldState::White);
        pf3.set_field_state(FieldPos { ring_index: 0, index: 1 }, FieldState::White);
        pf3.set_field_state(FieldPos { ring_index: 0, index: 0 }, FieldState::White);

        [pf1, pf2, pf3]
    }

    // TODO Might be wrong due to removing the immutable part?
    /// Places the amount of stones ion the playfield, starting on `start_index` from left to the right
    ///
    ///  - `amount_of_stones` is the recursion depth of this function
    pub fn distribute_stones_and_add(
        &mut self,
        stone_color: PlayerColor,
        amount_of_stones: i32,
        start_index: u32,
        set: &mut FnvHashSet<EfficientPlayField>,
    ) {
        if 0 < amount_of_stones {
            for i in start_index..24 {
                let current_field = FieldPos { ring_index: (i / 8) as usize, index: i % 8 };

                if self.get_field_state_at(current_field) != 0 {
                    continue;
                }

                let ring_backup = self.state[current_field.ring_index];
                self.state[current_field.ring_index] |=
                    <PlayerColor as Into<u16>>::into(stone_color) << (current_field.index * 2);

                set.insert(self.get_canon_form());

                if 23 < start_index {
                    return;
                }
                // Recursive call with one stones less to the next start_index
                else if 1 < amount_of_stones {
                    self.distribute_stones_and_add(stone_color, amount_of_stones - 1, i + 1, set);
                }
                self.state[current_field.ring_index] = ring_backup;
            }
        }
    }

    /// Generates all playflied configuration in which the black stones are enclosed by white stones
    /// These playfields count to the [generate_won_configs_white]
    ///
    /// First places 4 black fields onto the playfield and then distributes the further black ones on the field.
    /// After this, the method tries to enclose these generated black field with white ones by calling [enclose_black_stones]
    pub(crate) fn generate_black_enclosed_configs(max_stone_count: i32, won_set: &mut FnvHashSet<EfficientPlayField>) {
        let pf = EfficientPlayField::default();
        let mut black_only = FnvHashSet::<EfficientPlayField>::default();

        for i in 0..24 {
            let current_field = FieldPos { ring_index: (i / 8) as usize, index: i % 8 };

            let mut pf = pf;
            pf.state[current_field.ring_index] |= 2u16 << (current_field.index * 2);

            for j in (i + 1)..24 {
                let current_field = FieldPos { ring_index: (j / 8) as usize, index: j % 8 };

                let mut pf = pf;
                pf.state[current_field.ring_index] |= 2u16 << (current_field.index * 2);

                for k in (j + 1)..24 {
                    let current_field = FieldPos { ring_index: (k / 8) as usize, index: k % 8 };

                    let mut pf = pf;
                    pf.state[current_field.ring_index] |= 2u16 << (current_field.index * 2);

                    for l in (k + 1)..24 {
                        let current_field = FieldPos { ring_index: (l / 8) as usize, index: l % 8 };

                        let mut pf = pf;
                        pf.state[current_field.ring_index] |= 2u16 << (current_field.index * 2);

                        black_only.insert(pf.get_canon_form());

                        // Adding combinations of 4<= playfieds to the black only set
                        // 4 <= due to 3 can't be enclosed by white stones because of possible jumping
                        pf.distribute_stones_and_add(
                            PlayerColor::Black,
                            (max_stone_count - 4).max(0),
                            l + 1,
                            &mut black_only,
                        );
                    }
                }
            }
        }

        // Refactored to save initializations of the Hashset
        let mut enclosing_field_position_buffer = FnvHashSet::<FieldPos>::default();

        for mut playfield in black_only {
            playfield.enclose_black_stones(max_stone_count, won_set, &mut enclosing_field_position_buffer);
            enclosing_field_position_buffer.clear();
        }
    }

    /// Returns self with added white stones that enclose black stones,
    /// and if possible extra placements of left over white stones
    fn enclose_black_stones(
        &mut self,
        max_stone_count: i32,
        won_set: &mut FnvHashSet<EfficientPlayField>,
        enclosing_field_buffer: &mut FnvHashSet<FieldPos>,
    ) {
        self.get_placements_to_enclose_black(enclosing_field_buffer);
        let amount_of_white_moves_to_place = enclosing_field_buffer.len() as i32;

        if amount_of_white_moves_to_place <= max_stone_count {
            // places a white stone on all possible placements
            for FieldPos { ring_index, index: field_index } in enclosing_field_buffer.iter() {
                self.state[*ring_index] |= 1u16 << (field_index * 2);
            }

            //enclosure without extra stones placed
            won_set.insert(self.clone().get_canon_form());

            // if there are leftovers, all possible placements are done and added to the set
            let left_overs = max_stone_count - amount_of_white_moves_to_place;
            self.distribute_stones_and_add(PlayerColor::White, left_overs, 0, won_set);
        }
    }

    /// Adds FieldPos for enclosing a stone into the set
    fn add_stone_move_placements(start: FieldPos, direction: MoveDirection, enclose_pos: &mut FnvHashSet<FieldPos>) {
        match direction {
            MoveDirection::AcrossRings { target_ring_index } => {
                enclose_pos.insert(FieldPos { ring_index: target_ring_index, ..start })
            }
            MoveDirection::OnRing { target_field_index } => {
                enclose_pos.insert(FieldPos { index: target_field_index, ..start })
            }
        };
    }

    /// Adds all possible FieldPos with the correct placement of the white stone for the enclosure to the set
    pub fn get_placements_to_enclose_black(&self, all_enclose_pos: &mut FnvHashSet<FieldPos>) {
        for ring_index in 0..3 {
            for field_index in 0..8 {
                let current_field = FieldPos { ring_index, index: field_index };
                let current_field_state = self.get_field_state_at(current_field);

                if current_field_state == 0 {
                    continue;
                }

                // All possible enclose placements are added into the Set
                for (neighbor_index, neighbor_state) in self.get_neighbor_field_states(current_field) {
                    // Neighbor field state is empty - neighbor_index already are representational index (0 <= i < 16)
                    if neighbor_state == 0 {
                        EfficientPlayField::add_stone_move_placements(
                            current_field,
                            MoveDirection::OnRing { target_field_index: neighbor_index },
                            all_enclose_pos,
                        );
                    }
                }

                // Check for possible over-ring moves
                if (field_index % 2) == 0 {
                    let (next_rings_field_state, previous_rings_field_state) =
                        self.get_neigbor_rings_field_states(current_field);

                    match ring_index {
                        // Inner Ring
                        0 if next_rings_field_state == 0 => {
                            EfficientPlayField::add_stone_move_placements(
                                FieldPos { ring_index: 0, index: field_index },
                                MoveDirection::AcrossRings { target_ring_index: 1 },
                                all_enclose_pos,
                            );
                        }
                        // Mid Ring
                        1 => {
                            if previous_rings_field_state == 0 {
                                EfficientPlayField::add_stone_move_placements(
                                    FieldPos { ring_index: 1, index: field_index },
                                    MoveDirection::AcrossRings { target_ring_index: 0 },
                                    all_enclose_pos,
                                );
                            }

                            if next_rings_field_state == 0 {
                                EfficientPlayField::add_stone_move_placements(
                                    FieldPos { ring_index: 1, index: field_index },
                                    MoveDirection::AcrossRings { target_ring_index: 2 },
                                    all_enclose_pos,
                                );
                            }
                        }
                        // Outer Ring
                        2 if previous_rings_field_state == 0 => {
                            EfficientPlayField::add_stone_move_placements(
                                FieldPos { ring_index: 2, index: field_index },
                                MoveDirection::AcrossRings { target_ring_index: 1 },
                                all_enclose_pos,
                            );
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    // Needed for optimization of generating enclosed playfields
    // Returns amount of mills present of one color on the playfields
    fn get_total_amount_of_mills_and_double_mills(&self, color: PlayerColor) -> (usize, usize) {
        let mut mill_count: usize = 0;
        let mut double_mill_count: usize = 0;

        let mut lane_stone_count = [0; 4];
        for ring_index in 0..3 {
            for field_index in 0..8 {
                let current_field = FieldPos { ring_index, index: field_index };

                if field_index % 2 == 0 {
                    mill_count += self.get_mill_count(current_field, DirectionToCheck::OnRing) as usize;

                    let current_even_index_state = (self.state[ring_index] << (field_index * 2)) >> (field_index * 2);

                    if current_even_index_state == color.into() {
                        lane_stone_count[(field_index / 2) as usize] += 1;
                    }
                }

                // TODO passdas?
                if self.get_mill_count(current_field, DirectionToCheck::OnAndAcrossRings { player_color: color.into() })
                    == 2
                {
                    double_mill_count += 1;
                }
            }
        }

        for elem in lane_stone_count {
            if elem == 3 {
                mill_count += 1;
            }
        }

        (mill_count, double_mill_count)
    }
}
