#region Define the Windows Form
[Void][Reflection.Assembly]::LoadWithPartialName("System.Windows.Forms")

$Form1 = New-Object System.Windows.Forms.Form
$label1 = New-Object System.Windows.Forms.Label
$label2 = New-Object System.Windows.Forms.Label
$txtInputText = New-Object System.Windows.Forms.TextBox
$txtInputNumber = New-Object System.Windows.Forms.TextBox
$btnAccept = New-Object System.Windows.Forms.Button
$label3 = New-Object System.Windows.Forms.Label
$btnCancel = New-Object System.Windows.Forms.Button
$SuspendLayout
#
# label1
#
$label1.AutoSize = $true
$label1.Location = New-Object System.Drawing.Point(23, 36)
$label1.Name = "label1"
$label1.Size = New-Object System.Drawing.Size(34, 13)
$label1.TabIndex = 0
$label1.Text = "String"
#
# label2
#
$label2.AutoSize = $true
$label2.Location = New-Object System.Drawing.Point(13, 62)
$label2.Name = "label2"
$label2.Size = New-Object System.Drawing.Size(44, 13)
$label2.TabIndex = 1
$label2.Text = "Number"
#
# txtInputText
#
$txtInputText.Location = New-Object System.Drawing.Point(63, 33)
$txtInputText.Name = "txtInputText"
$txtInputText.Size = New-Object System.Drawing.Size(100, 20)
$txtInputText.TabIndex = 0
#
# txtInputNumber
#
$txtInputNumber.Location = New-Object System.Drawing.Point(63, 59)
$txtInputNumber.Name = "txtInputNumber"
$txtInputNumber.Size = New-Object System.Drawing.Size(100, 20)
$txtInputNumber.TabIndex = 1
$txtInputNumber.Text = "75000"
#
# btnAccept
#
$btnAccept.DialogResult = [System.Windows.Forms.DialogResult]::OK
$btnAccept.Location = New-Object System.Drawing.Point(16, 94)
$btnAccept.Name = "btnAccept"
$btnAccept.Size = New-Object System.Drawing.Size(75, 23)
$btnAccept.TabIndex = 2
$btnAccept.Text = "Accept"
$btnAccept.UseVisualStyleBackColor = $true
$btnAccept.add_Click({$rc="Accept"; $Form1.Close()})
#
# label3
#
$label3.AutoSize = $true
$label3.Location = New-Object System.Drawing.Point(13, 9)
$label3.Name = "label3"
$label3.Size = New-Object System.Drawing.Size(173, 13)
$label3.TabIndex = 5
$label3.Text = "Please input a string and a number:"
#
# btnCancel
#
$btnCancel.DialogResult = [System.Windows.Forms.DialogResult]::Cancel
$btnCancel.Location = New-Object System.Drawing.Point(97, 94)
$btnCancel.Name = "btnCancel"
$btnCancel.Size = New-Object System.Drawing.Size(75, 23)
$btnCancel.TabIndex = 3
$btnCancel.Text = "Cancel"
$btnCancel.UseVisualStyleBackColor = $true
#
# Form1
#
$Form1.AcceptButton = $btnAccept
$Form1.CancelButton = $btnCancel
$Form1.ClientSize = New-Object System.Drawing.Size(196, 129)
$Form1.ControlBox = $false
$Form1.Controls.Add($btnCancel)
$Form1.Controls.Add($label3)
$Form1.Controls.Add($btnAccept)
$Form1.Controls.Add($txtInputNumber)
$Form1.Controls.Add($txtInputText)
$Form1.Controls.Add($label2)
$Form1.Controls.Add($label1)
$Form1.Name = "Form1"
$Form1.Text = "RosettaCode"

#endregion Define the Windows Form

### Show the input form
$f = $Form1.ShowDialog()
if ( $f -eq [System.Windows.Forms.DialogResult]::Cancel ) { "User selected Cancel" }
else { "User entered `"{0}`" for the text and {1} for the number" -f $txtInputText.Text, $txtInputNumber.Text }
