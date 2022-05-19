options short circuit

private define
  mv_screenClosed smallint

main
  define
    lv_windowTitle string,
    lv_windowRoot ui.Window,
    edit_field string

  let lv_windowTitle = "Example Form Program"

  # open window
  open window w with form "FormProgram"

  call closeDefaultScreen()
  # get the window root and set the title
  let lv_windowRoot = ui.Window.getCurrent()
  if (lv_windowRoot is not null) then
    call lv_windowRoot.setText(lv_windowTitle)
  end if

  input by name edit_field

    on action accept
      exit input
    
    on action cancel
      exit input

    on action close
      exit input

  end input
  
  close window w
    
end main

private function closeDefaultScreen()
  define 
    lv_uiRoot om.DomNode,
    lv_nodeList om.NodeList,
    lv_nodeListCount smallint

  if (mv_screenClosed is null) or (not mv_screenClosed) then
    let lv_uiRoot = ui.Interface.getRootNode()
    if (lv_uiRoot is not null) then
      let lv_nodeList = lv_uiRoot.selectByPath("//Window[@name=\"screen\"]")
      let lv_nodeListCount = lv_nodeList.getLength()

      if (lv_nodeListCount > 0) then
        close window screen
        let mv_screenClosed = TRUE
      end if
    end if
  end if

end function