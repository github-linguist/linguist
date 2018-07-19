(import 'javax.swing.JOptionPane)
(let [number (-> "Enter an Integer"
                 JOptionPane/showInputDialog
                 Integer/parseInt)
      string (JOptionPane/showInputDialog "Enter a String")]
  [number string])
