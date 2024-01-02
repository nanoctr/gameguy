pub trait ByteIterExtensions: Iterator<Item = u8> {
    fn read_u16(&mut self) -> Result<u16, String> {
        let upper = self
            .next()
            .ok_or_else(|| "No more bytes to read!".to_owned())?;

        let lower = self
            .next()
            .ok_or_else(|| "No more bytes to read!".to_owned())?;

        Ok(((upper as u16) << 8) | (lower as u16))
    }
}

impl<T> ByteIterExtensions for T where T: Iterator<Item = u8> {}
