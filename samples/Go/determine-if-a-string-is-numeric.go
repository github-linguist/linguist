import "strconv"

func IsNumeric(s string) bool {
    _, err := strconv.ParseFloat(s, 64)
    return err == nil
}
