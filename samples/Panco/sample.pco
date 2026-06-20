# Enable database relative configuration
    [db}database/panco.db]
  
    # Import the seeded graphical Panco extension
    import# graphical
  
    # Define click handler
    delta on_btn_clicked() {
        makeword("Greeting: Button was clicked inside GUI!")
    }
  
    # Construct GUI window and widgets
    [allow}window\create_window("Panco Graphic App")]
    add_label(window, "Hello from the Graphical Extension!")
    add_button(window, "Click Me", "on_btn_clicked")
  
    # Start loop
    start_gui(window)
