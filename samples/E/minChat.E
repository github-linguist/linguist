# from
# http://wiki.erights.org/wiki/Walnut/Secure_Distributed_Computing/Auditing_minChat
pragma.syntax("0.9")
to send(message) {
    when (friend<-receive(message)) -> {
        chatUI.showMessage("self", message)
    } catch prob {chatUI.showMessage("system", "connection lost")}
}
to receive(message) {chatUI.showMessage("friend", message)}
to receiveFriend(friendRcvr) {
    bind friend := friendRcvr        
    chatUI.showMessage("system", "friend has arrived")
}
to save(file) {file.setText(makeURIFromObject(chatController))}
to load(file) {
    bind friend := getObjectFromURI(file.getText())
    friend <- receiveFriend(chatController)
}
