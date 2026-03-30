pub fn apply_string_escapes(input: &str) -> String {
    input.replace("\\\"", "\"")
}
