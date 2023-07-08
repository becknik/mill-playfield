use std::{
    collections::VecDeque,
    fs::File,
    io::{BufRead, BufReader, BufWriter, Write},
};

use fnv::FnvHashSet;
use smallvec::SmallVec;

use crate::PlayerColor;

use super::{DirectionToCheck, FieldPos};
use super::{EfficientPlayField, RelativePosition};

pub mod move_simulations;
pub mod start_set_generation;

#[cfg(test)]
pub mod unit_tests;

const TO_TAKE_VEC_SIZE: usize = 64;

impl EfficientPlayField {
    /// Counts & returns the amount of stones on the whole plazfield
    pub fn get_stone_count_of(&self, player_color: PlayerColor) -> u32 {
        let mut stone_counter = 0;

        for ring_index in 0..3 {
            for field_index in 0..8 {
                let current_field_state = self.get_field_state_at(FieldPos { ring_index, index: field_index });

                if current_field_state == (<PlayerColor as Into<u16>>::into(player_color) << (field_index * 2)) {
                    stone_counter += 1;
                }
            }
        }
        stone_counter
    }

    //machts hier nicht sinn vllt doch player_color andersrum zu machen?

    /// Returns the FieldPos field coordinates of stones that can be taken by the player with player_color
    /// Therefore, the SmallVec returns only fields with stones of the color !player_color
    pub fn get_fields_to_take_by(&self, player_color: PlayerColor) -> SmallVec<[FieldPos; TO_TAKE_VEC_SIZE]> {
        let mut all_stones_to_take_pos = SmallVec::<[FieldPos; TO_TAKE_VEC_SIZE]>::new();
        let mut not_in_mill_pos = SmallVec::<[FieldPos; TO_TAKE_VEC_SIZE]>::new();

        let opponent_player_color_rep: u16 = (!player_color).into();

        for ring_index in 0..3 {
            for field_index in 0..8 {
                let current_field_state = self.get_field_state_at(FieldPos { ring_index, index: field_index });

                if current_field_state == (opponent_player_color_rep << (2 * field_index)) {
                    all_stones_to_take_pos.push(FieldPos { ring_index, index: field_index });

                    // If the opponent has no mill on this field, add this field to the appropriate set
                    if 0 == self.get_mill_count(
                        FieldPos { ring_index, index: field_index },
                        DirectionToCheck::OnAndAcrossRings { player_color: opponent_player_color_rep },
                    ) {
                        not_in_mill_pos.push(FieldPos { ring_index, index: field_index });
                    }
                }
            }
        }

        // If all stones are in mills, stones from mills can be taken
        if not_in_mill_pos.is_empty() {
            all_stones_to_take_pos
        } else {
            not_in_mill_pos
        }
    }

    /// Returns the fields which are free to place a stone upon.
    pub fn get_empty_fields(&self) -> SmallVec<[FieldPos; 19]> {
        let mut empty_fields = SmallVec::<[FieldPos; 19]>::new();

        for ring_index in 0..3 {
            for field_index in 0..8 {
                let current_field_state = self.get_field_state_at(FieldPos { ring_index, index: field_index });

                if current_field_state == 0 {
                    empty_fields.push(FieldPos { ring_index, index: field_index });
                }
            }
        }
        empty_fields
    }

    pub fn generate_won_configs_black_and_white(
        max_stone_count: i32,
    ) -> (FnvHashSet<EfficientPlayField>, FnvHashSet<EfficientPlayField>) {
        let mut won_set = EfficientPlayField::generate_start_won_configs_white(max_stone_count);
        let mut lost_set = FnvHashSet::<EfficientPlayField>::default();

        let mut work_queue: VecDeque<EfficientTreePlayField> =
            won_set.iter().map(|pf| EfficientTreePlayField { playfield: *pf, niveau: 0 }).collect();

        while let Some(EfficientTreePlayField { playfield: mut current_pf, niveau: tree_level_bottom_up }) =
            work_queue.pop_front()
        {
            // White moved last
            if tree_level_bottom_up % 2 == 0 {
                // Every backward move is going to be added:
                current_pf
                    .get_backward_moves(PlayerColor::White, max_stone_count)
                    .iter_mut()
                    .map(|pf| pf.get_canon_form())
                    .filter(|pf_canon| won_set.insert(*pf_canon))
                    .for_each(|previously_unknown_pf| {
                        work_queue.push_back(EfficientTreePlayField {
                            playfield: previously_unknown_pf,
                            niveau: tree_level_bottom_up + 1,
                        })
                    });
            }
            //Black moved last
            else {
                for mut backward_move in current_pf.get_backward_moves(PlayerColor::Black, max_stone_count) {
                    //backward_move = backward_move.get_canon_form();

                    let is_any_backward_forward_move_unknown = backward_move
                        .get_forward_moves(PlayerColor::Black)
                        .iter_mut()
                        .map(|pf| pf.get_canon_form())
                        .any(|pf_canon| !won_set.contains(&pf_canon));

                    // Making a backward-move when black is on turn, we have to make forward moves again to determine if
                    // the playfield is lost or won. Therefore, when on of those backward-forward playfield win-lost state
                    // is not determined, we can't reason about the backward move
                    if is_any_backward_forward_move_unknown {
                        continue;
                    }

                    // Adds the inverted backward_playfield to lost_set
                    let insert_playfield = backward_move.invert_playfields_stone_colors().get_canon_form();

                    if lost_set.insert(insert_playfield) {
                        work_queue.push_back(EfficientTreePlayField {
                            playfield: backward_move,
                            niveau: tree_level_bottom_up + 1,
                        });
                    }
                }
            }
        }
        (lost_set, won_set)
    }

    // Alignment to LSB is done by this function
    pub fn get_neigbor_rings_field_states(&self, field: FieldPos) -> (u16, u16) {
        let next_ring_index = EfficientPlayField::get_ring_index(field.ring_index, RelativePosition::Next);
        let next_rings_field_state =
            self.get_field_state_at(FieldPos { ring_index: next_ring_index, ..field }) >> (2 * field.index);

        let previous_ring_index = EfficientPlayField::get_ring_index(field.ring_index, RelativePosition::Previous);
        let previous_rings_field_state =
            self.get_field_state_at(FieldPos { ring_index: previous_ring_index, ..field }) >> (2 * field.index);
        (next_rings_field_state, previous_rings_field_state)
    }

    pub fn invert_playfields_stone_colors(&self) -> EfficientPlayField {
        let mut current_playfield = *self;

        for ring_index in 0..3 {
            for field_index in 0..8 {
                match self.get_field_state_at(FieldPos { ring_index, index: field_index }) >> (field_index * 2) {
                    1u16 => {
                        current_playfield.state[ring_index] = (current_playfield.state[ring_index]
                            & !(3u16 << (field_index * 2)))
                            | (2u16 << (field_index * 2))
                    }
                    2u16 => {
                        current_playfield.state[ring_index] = (current_playfield.state[ring_index]
                            & !(3u16 << (field_index * 2)))
                            | (1u16 << (field_index * 2))
                    }
                    _ => {}
                }
            }
        }
        current_playfield
    }
}

struct EfficientTreePlayField {
    playfield: EfficientPlayField,
    niveau: u16,
}

/// Takes two provided reference files, namely `output.txt` (containing nash-values 0..3) and
/// `input_felder.txt` ( containing string-encoded playfield configs)and compares their content to the result of
/// the computation for the won and lost sets elements.
pub fn compare_to_reference_data(input_felder: &str, output: &str, max_stone_count: i32) {
    let input_felder_txt = File::open(input_felder).unwrap();
    let reader_input = BufReader::new(input_felder_txt);

    let output_txt = File::open(output).unwrap();
    let mut reader_output = BufReader::new(output_txt);

    let dbg_file = File::create("dbg_output.txt").unwrap();
    let mut writer_dbg = BufWriter::new(dbg_file);

    let (lost, won) = EfficientPlayField::generate_won_configs_black_and_white(max_stone_count);

    println!("\n\nWON: {} --- LOST: {}\n\n", won.len(), lost.len());

    let mut counter_failures = 0;
    for line_content in reader_input.lines() {
        let mut playfield = EfficientPlayField::from_coded(&line_content.unwrap());
        let canonical_form = playfield.get_canon_form();

        // Reading the associated line to the playfield from output.txt:
        let mut output_input_buffer = String::default();
        reader_output.read_line(&mut output_input_buffer).unwrap();

        let nash_value = if won.contains(&canonical_form) {
            2
        } else if lost.contains(&canonical_form) {
            0
        } else {
            1
        };
        let nash_value_reference = output_input_buffer.trim().parse::<i32>().unwrap();

        // A run takes a while, so seeing the problematic field directly may be considered helpful
        if nash_value_reference != nash_value {
            counter_failures += 1;
            println!("{playfield}");
            writeln!(
                writer_dbg,
                "program: {nash_value} - reference: {nash_value_reference} - playfield: {}",
                playfield.to_string_representation()
            )
            .unwrap()

            //assert_eq!(nash_value_reference, nash_value);
        }
    }
    println!("Amount of failures: {counter_failures}");
}
