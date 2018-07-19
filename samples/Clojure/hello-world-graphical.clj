(ns experimentation.core
  (:import (javax.swing JOptionPane JFrame JTextArea JButton)
     (java.awt FlowLayout)))

(JOptionPane/showMessageDialog nil "Goodbye, World!")
(let [button (JButton. "Goodbye, World!")
      window (JFrame. "Goodbye, World!")
      text (JTextArea. "Goodbye, World!")]
  (doto window
    (.setLayout (FlowLayout.))
    (.add button)
    (.add text)
    (.pack)
    (.setDefaultCloseOperation (JFrame/EXIT_ON_CLOSE))
    (.setVisible true)))
