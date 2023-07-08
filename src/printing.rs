use std::fmt::Display;

use super::EfficientPlayField;

impl Display for EfficientPlayField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut row_counter = 7;

        f.write_str("\n")?;

        let (a, b, c) =
            (self.get_field_state_char(2, 7), self.get_field_state_char(2, 0), self.get_field_state_char(2, 1));
        writeln!(f, "\t{}|  {}------------{}------------{}", row_counter, a, b, c)?;

        row_counter -= 1;
        f.write_str("\t |  |            |            |\n")?;

        let (a, b, c) =
            (self.get_field_state_char(1, 7), self.get_field_state_char(1, 0), self.get_field_state_char(1, 1));
        writeln!(f, "\t{}|  |   {}--------{}--------{}   |", row_counter, a, b, c)?;
        row_counter -= 1;

        f.write_str("\t |  |   |        |        |   |\n")?;

        let (a, b, c) =
            (self.get_field_state_char(0, 7), self.get_field_state_char(0, 0), self.get_field_state_char(0, 1));
        writeln!(f, "\t{}|  |   |   {}----{}----{}   |   |", row_counter, a, b, c)?;
        row_counter -= 1;

        f.write_str("\t |  |   |   |         |   |   |\n")?;

        let (a, b, c) =
            (self.get_field_state_char(2, 6), self.get_field_state_char(1, 6), self.get_field_state_char(0, 6));
        write!(f, "\t{}|  {}---{}---{}", row_counter, a, b, c)?;

        let (a, b, c) =
            (self.get_field_state_char(0, 2), self.get_field_state_char(1, 2), self.get_field_state_char(2, 2));
        writeln!(f, "         {}---{}---{}", a, b, c)?;
        row_counter -= 1;

        f.write_str("\t |  |   |   |         |   |   |\n")?;

        let (a, b, c) =
            (self.get_field_state_char(0, 5), self.get_field_state_char(0, 4), self.get_field_state_char(0, 3));
        writeln!(f, "\t{}|  |   |   {}----{}----{}   |   |", row_counter, a, b, c)?;
        row_counter -= 1;

        f.write_str("\t |  |   |        |        |   |\n")?;

        let (a, b, c) =
            (self.get_field_state_char(1, 5), self.get_field_state_char(1, 4), self.get_field_state_char(1, 3));
        writeln!(f, "\t{}|  |   {}--------{}--------{}   |", row_counter, a, b, c)?;
        row_counter -= 1;

        f.write_str("\t |  |            |            |\n")?;

        let (a, b, c) =
            (self.get_field_state_char(2, 5), self.get_field_state_char(2, 4), self.get_field_state_char(2, 3));
        writeln!(f, "\t{}|  {}------------{}------------{}", row_counter, a, b, c)?;

        f.write_str("\t   ____________________________\n")?;
        f.write_str("\t    A   B   C    D    E   F   G\n")?;

        f.write_fmt(format_args!(
            "\n\tRing 0: {:016b}\n\tRing 1: {:016b}\n\tRing 2: {:016b}\n",
            self.state[0], self.state[1], self.state[2]
        ))
    }
}

impl EfficientPlayField {
    /// Converts the state of the specified index to a char (00 to '·', 01 to '●' & 10 to '○')
    fn get_field_state_char(&self, ring_index: usize, index: u32) -> char {
        match ((self.state[ring_index] & (3u16 << (index * 2))) >> (index * 2)) as u16 {
            0u16 => '·',
            1u16 => '●',
            2u16 => '○',
            _ => panic!(),
        }
    }

    pub fn to_string(&self) -> String {
        let bit_mask = 3u16;
        let mut output_string = String::new();

        for i in 0..3 {
            for j in 0..8 {
                let current_element = (self.state[i] & (bit_mask << (2 * j))) >> (2 * j);

                match current_element {
                    0x0000 => output_string.push('E'),
                    0x0001 => output_string.push('W'),
                    0x0002 => output_string.push('B'),
                    _ => {}
                }
            }
        }
        output_string
    }
}
