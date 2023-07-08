use std::{
    fs::File,
    io::{BufWriter, Write},
};

use fnv::FnvHashSet;

use crate::EfficientPlayField;

use super::compare_to_reference_data;

#[test]
fn number_of_base_won_playfields_is_correct_test() {
    let incremental_won_set = EfficientPlayField::generate_start_won_configs_white(9);

    let mut enclosed_won_set = FnvHashSet::<EfficientPlayField>::default();
    EfficientPlayField::generate_black_enclosed_configs(9, &mut enclosed_won_set);

    assert_eq!(7825361, (incremental_won_set.len() - enclosed_won_set.len()));
    assert_eq!(567794, enclosed_won_set.len());
    assert_eq!(8393155, incremental_won_set.len());

    // TODO Shouldn't we get less because we filter out some of the unreachable fields?
}

#[test]
fn t3vs3_all_won_loose_playfields_count_correct() {
    let (lost, won) = EfficientPlayField::generate_won_configs_black_and_white(3);

    println!("\n\nWON: {} --- LOST: {}\n\n", won.len(), lost.len());

    let dbg_file = File::create("dbg_output.txt").unwrap();
    let mut dbg_writer = BufWriter::new(dbg_file);
    won.iter().for_each(|pf| {
        dbg_writer.write_fmt(format_args!("{}\n", pf.to_string_representation())).unwrap();
    });

    assert_eq!(140621, won.len());
    assert_eq!(28736, lost.len());
}

#[test]
fn t3vs3_all_won_loose_set_correct() {
    compare_to_reference_data("input_felder_3vs3.txt", "output_3vs3.txt", 3);
}

#[test]
fn t5vs5_all_won_loose_set_correct() {
    compare_to_reference_data("input_felder_5vs5.txt", "output_5vs5.txt", 5);
}

#[test]
fn t5vs5_all_won_loose_set_correct_large() {
    compare_to_reference_data("input_felder_5vs5_large.txt", "output_5vs5_large.txt", 5);
}

#[test]
fn t5vs5_run_won_loose_set_generation() {
    EfficientPlayField::generate_won_configs_black_and_white(5);
}

#[test]
fn t9vs9_run_won_loose_set_generation() {
    EfficientPlayField::generate_won_configs_black_and_white(9);
}
