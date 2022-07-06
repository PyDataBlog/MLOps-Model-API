func convertToTitle(n int) string {
    execl := []string{"A", "B","C","D","E","F","G","H", "I", "J","K","L","M","N","O", "P", "Q","R","S","T","U","V","W","X","Y","Z"}
    var temp string
    for n >0 {
        temp = execl[(n-1)%26] +temp
        n = (n-1) /26
    }
    return temp
}
//------------------------------
func convertToTitle(n int) string {
    var res []byte
    for n > 0 {
        n--
        k := n % 26
        res = append(res, byte('A') + byte(k))
        n /= 26
    }
    for i, j := 0, len(res)-1; i < j; i, j = i+1, j-1 {
        res[i], res[j] = res[j], res[i]
    }
    return string(res)
}
