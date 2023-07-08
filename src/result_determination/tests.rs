//! Some tests for me to not accidentally mess stuff up when refactoring/ improving the program

use std::collections::HashSet;

use crate::game::{efficient_state::EfficientPlayField, PlayerColor};
/*
#[test]
fn test_invert_playfield() {
    let test_string = "WWWWBBBBEEEEWWWWBBBBEEEE";
    let test_playfield = EfficientPlayField::from_coded(test_string);

    println!("{test_playfield}");
    println!("{}", test_playfield.invert_playfields_stone_colors());
}

#[test]
fn test_get_fields_to_take() {
    let test_string = "WEEEBEEEWEEEBEEEWEEEBBWB";
    let test_playfield = EfficientPlayField::from_coded(test_string);

    println!("\n--- Initial Playfield ---\n");
    println!("{test_playfield}");
    println!("\n--- Fields with legal stones taken ---\n");

    let fields = test_playfield.get_fields_to_take_by(PlayerColor::Black);

    let mut i = 0;
    fields
        .iter()
        .map(|field| {
            let mut new_pf = test_playfield.clone();
            new_pf.state[field.ring_index] &= !(3u16 << (field.field_index * 2));
            new_pf
        })
        .for_each(|pf| {
            println!("> PlayField on Index {i}:\n{pf}");
            i += 1;
        });
}

#[test]
fn test_get_fields_to_place() {
    // WWEEEEEWEEEEEEEEEEEEEEEE
    // WWWWBBBBEEEEWWWWBBBBEEEE

    let test_string = "WWWWBBBBEEEEWWWWBBBBEEEE";
    let test_playfield = EfficientPlayField::from_coded(test_string);

    println!("\n--- Initial Playfield ---\n");
    println!("{test_playfield}");
    println!("\n--- Fields with legal stones placed ---\n");

    let vec = test_playfield.get_empty_field_bitmasks(PlayerColor::White);

    let mut i = 0;
    vec.iter()
        .map(|tuple| {
            let mut new_pf = test_playfield.clone();
            new_pf.state[tuple.0] |= tuple.1;
            new_pf
        })
        .for_each(|pf| {
            println!("> PlayField on Index {i}:\n{pf}");
            i += 1;
        });
}

#[test]
fn test_get_forward_moves() {
    // Default-Move-Pattern:        "WEEEEEEEEWEEWEEEEEEEEEWE"
    // Move-Into-Muehle-Pattern:    "WEWEEEEWEEEEEBEBEBEEEEEE"

    let test_string = "WEWEEEEWEEEEEBEBEBBEEEEE";
    let mut test_playfield = EfficientPlayField::from_coded(test_string);

    println!("\n--- Initial Playfield ---\n");
    println!("{test_playfield}");
    println!("\n--- Fields with simulated moves ---\n");

    let vec = test_playfield.get_forward_moves(PlayerColor::White);

    let mut i = 0;
    vec.iter().for_each(|pf| {
        println!("> PlayField on Index {i}:\n{pf}");
        i += 1;
    });
}

#[test]
fn test_get_backward_moves() {
    // Default-Move-Pattern:        "WEEEEEEEEWEEWEEEEEEEEEWE"
    // Move-Out-Of-Muehle-Pattern:  "WWEEEEEWEEEEEEEEEEEEEEEE"

    let test_string = "WWEEEEEWEEEEEBBEEEEBBEEE";
    let mut test_playfield = EfficientPlayField::from_coded(test_string);

    println!("\n--- Initial Playfield ---\n");
    println!("{test_playfield}");
    println!("\n--- Fields with simulated moves ---\n");

    let vec = test_playfield.get_backward_moves(PlayerColor::White);

    let mut i = 0;
    vec.iter().for_each(|pf| {
        println!("> PlayField on Index {i}:\n{pf}");
        i += 1;
    });
}

#[test]
fn test_generate_won_configurations_non_enclosing() {
    let won_set = EfficientPlayField::generate_white_won_configurations(9);
    println!("{}", won_set.len())

    /*let mut i = 0;
    won_set.iter()
        .filter(|pf| {
            let mut white_stones_count = 0;

            for i in 0..24 {
                let ring_index = (i / 8) as usize;
                let field_index = i % 8;

                let current_index_state = pf.state[ring_index] & (3u16 << (field_index * 2));
                if current_index_state == (1u16 << (field_index * 2)) {
                    white_stones_count += 1;
                }
            }

            white_stones_count == 5
        })
        .for_each(|pf| {
            println!("> PlayField on Index {i}:\n{pf}");
            i += 1;
        }
    );*/
}

#[test]
fn test_generate_enclosed_won_set() {
    let mut won_set = HashSet::<EfficientPlayField>::new();
    EfficientPlayField::add_white_won_configurations_enclosed(9, &mut won_set);

    println!("{}", won_set.len());

    /* let mut i = 0;
    won_set.iter().for_each(|pf| {
        println!("> PlayField on Index {i}:\n{pf}");
        i += 1;
    }); */

    /* let mut i = 0;
    won_set.iter()
        .filter(|pf| {
            let mut black_stones_count = 0;

            for i in 0..24 {
                let ring_index = (i / 8) as usize;
                let field_index = i % 8;

                let current_index_state = pf.state[ring_index] & (3u16 << (field_index * 2));
                if current_index_state == (2u16 << (field_index * 2)) {
                    black_stones_count += 1;
                }
            }

            black_stones_count == 9
        })
        .for_each(|pf| {
            println!("> PlayField on Index {i}:\n{pf}");
            i += 1;
        }
    ); */
}

#[test]
fn test_generate_won_set() {
    let mut won_set = EfficientPlayField::generate_white_won_configurations(9);
    EfficientPlayField::add_white_won_configurations_enclosed(9, &mut won_set);

    println!("{}", won_set.len());
}

#[test]
fn test_generate_all_won_playfields_9() {
    let (won_set, _lost_set) = EfficientPlayField::generate_won_configs_black_and_white(9);
    println!("{}", won_set.len());
}

#[test]
fn test_generate_all_won_playfields_3() {
    let (won_set, lost_set) = EfficientPlayField::generate_won_configs_black_and_white(3);
    println!("Won: {}", won_set.len());
    println!("Lost: {}", lost_set.len());
}

#[test]
fn test_input_game_state_decider_5() {
    EfficientPlayField::input_game_state_decider(3);
}

#[test]
fn output3_dbg_test() {
    let failure_playfield_configs = [
        "EEBEEWEEEEBEEWEEEEBEWEEE",
        "BWEEEEEEBEWEEEEEBWEEEEEE",
        "EEEWEBBBWEEEEEEEEEEEEEWE",
        "EEEWEEEWEEEEWEEEBBEEEEEB",
        "EEWEEEEWBBEEEEEBEEEEEEEW",
        "EEEEEEEEEEEWEBBBWEWEEEEE",
        "EEEEWEEWWEEEEEEEEEEBBBEE",
        "BEEEWEWEBWEEEEEEBEEEEEEE",
        "WEEEEEEEEEEEWEEEBBEWEEEB",
        "EEEEWEEEEWEEEEEEWEEEEBBB",
        "BWEEEEEEBEEEEWEEBEWEEEEE",
        "WEEEEEEEBBEEWEEBEEEWEEEE",
        "EEEEEEEEWWEBBBEWEEEEEEEE",
    ];

    let mut i = 0;
    failure_playfield_configs.iter().for_each(|pf| {
        let pf = EfficientPlayField::from_coded(pf);
        println!("> PlayField on Index {i}:\n{pf}");
        i += 1;
    });
}*/
