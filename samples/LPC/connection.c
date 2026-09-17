/** 
 * query connection properties for a player
 * @return a union type of either an int or an array of strings
 */
public <int|string*>* queryConnectionProperties() {
    object p = this_player();
    if (!p) return 0;
    
    object conn = p->queryConnection();
    return conn->queryProperties();    
}
