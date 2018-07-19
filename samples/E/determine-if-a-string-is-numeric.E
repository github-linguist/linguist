def isNumeric(specimen :String) {
    try {
        <import:java.lang.makeDouble>.valueOf(specimen)
        return true
    } catch _ {
       return false
    }
}
