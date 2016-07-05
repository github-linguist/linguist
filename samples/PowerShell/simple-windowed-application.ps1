$count = 0

New-Window {
    New-StackPanel {
        New-TextBlock "There have been no clicks yet" `
            -On_Loaded {
                Set-Variable tb $this
            }
        New-Button "Click me" `
            -On_Click {
                $count++
                $tb.Text = $count
            }
    }
} -SizeToContent WidthAndHeight -Show
