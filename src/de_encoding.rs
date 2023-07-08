use super::EfficientPlayField;

impl EfficientPlayField {
    /// Reads a input str containing 24 chars consisting of 'E' = empty = 0b00, 'W' = white = 0b01 or 'B' = black = 0b10
    /// and constructs a [EfficientPlayField] instance from it.
    /// The input string starts coding the outest rings middle top field state and then circles right from outer to inner rect rings
    pub fn from_coded(input: &str) -> EfficientPlayField {
        //assert!(input.len() == 24);

        let mut play_field_state: u64 = 0u64;
        input.chars().enumerate().for_each(|(i, c)| match c {
            'E' => {}
            'W' => play_field_state |= 1u64 << (i * 2),
            'B' => play_field_state |= 2u64 << (i * 2),
            _ => panic!(),
        });

        let mut new_state = [0u16; 3];
        // The input starts with the outest ring
        new_state[2] = play_field_state as u16;
        new_state[1] = (play_field_state >> 16) as u16;
        new_state[0] = (play_field_state >> 32) as u16;

        /* 1 hour debugging because I wrote a test in a tired state in the Schienenersatzverkehr
        BBEEEEEB_EEEEWEWW_BWWEEEBE
        0000000000000000_0010000000010110_0101000100000000_1000000000001010
        Outer Ring : 1000000000001010 */

        EfficientPlayField { state: new_state }
    }

    /// Constructs the string described in [from_coded] from the current play field instance
    pub fn to_string_representation(&self) -> String {
        let mut state_formatted = String::with_capacity(24);

        for ring_index in 0..3 {
            let ring_state = self.state[ring_index];
            for i in (0..16).step_by(2) {
                // Mask out the last 2 bits for each field of the ring
                match (ring_state >> i) & 0x03u16 {
                    0u16 => state_formatted.push('E'),
                    1u16 => state_formatted.push('W'),
                    2u16 => state_formatted.push('B'),
                    _ => panic!(),
                }
            }
        }

        state_formatted
    }
}
