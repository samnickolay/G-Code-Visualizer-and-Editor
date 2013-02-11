Public Class Form1
    Dim Xmax, Ymax As Single
    Dim Xmin As Single = 10000
    Dim Ymin As Single = 10000
    Dim XCoordinates()() As Single = New Single()() {}
    Dim YCoordinates()() As Single = New Single()() {}
    Dim Xclick, Yclick As Single
    Dim ImageRatio, Xoffset, Yoffset As Single
    Dim fileLoaded As Boolean = False
    Dim Gcode, OriginalCode, testCode As String
    Dim letterCuts(26)(,)

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Try
            OpenFileDialog1.ShowDialog()
            If OpenFileDialog1.CheckFileExists Then
                Gcode = System.IO.File.ReadAllText(OpenFileDialog1.FileName, System.Text.Encoding.Default)
                RichTextBox1.Text = Gcode
                OriginalCode = Gcode
                fileLoaded = True
                Button2.Enabled = True
                Button3.Enabled = True
                Button4.Enabled = True
                Button5.Enabled = True
            Else
                MsgBox("File Does not Exist!")
            End If
            Dim CutLines()() As String = ComputeLines()
            DrawLines()
        Catch ex As Exception
            MsgBox("Could not open file")
        End Try

    End Sub

    Private Function ComputeLines()
        Dim inText As String = RichTextBox1.Text
        Dim splits = inText.Split("(")
        Dim profiles(100) As String
        Dim profileNum As Integer = 0

        For i As Integer = 0 To splits.Length - 1
            If splits(i).StartsWith("p") Or splits(i).StartsWith("t") Then
                profiles(profileNum) = splits(i)
                profileNum = profileNum + 1

            End If
        Next
        Array.Resize(profiles, profileNum)

        Dim roughLines()() As String = New String(profileNum)() {}
        Dim routeLines()() As String = New String(profileNum)() {}
        Array.Resize(XCoordinates, profileNum)
        Array.Resize(YCoordinates, profileNum)

        For i As Integer = 0 To profileNum - 1
            If profiles(i).StartsWith("t") Then
                roughLines(i) = profiles(i).Split("G")
                Dim linecounter As Integer = 0
                For j As Integer = 0 To roughLines(i).Length - 1
                    If roughLines(i)(j).Contains("X") And roughLines(i)(j).Contains("Y") Then
                        Array.Resize(routeLines(i), roughLines(i).Length)
                        routeLines(i)(linecounter) = roughLines(i)(j) + " "
                        Dim Xindex As Integer = roughLines(i)(j).IndexOf("X")
                        Dim Yindex As Integer = roughLines(i)(j).IndexOf("Y")

                        Dim Xstr = CSng(routeLines(i)(linecounter).Substring(Xindex + 1, (routeLines(i)(linecounter).IndexOf(" ", Xindex) - Xindex - 1)))
                        Dim Ystr = CSng(routeLines(i)(linecounter).Substring(Yindex + 1, (routeLines(i)(linecounter).IndexOf(" ", Yindex) - Yindex - 1)))

                        Array.Resize(XCoordinates(i), linecounter + 1)
                        Array.Resize(YCoordinates(i), linecounter + 1)
                        XCoordinates(i)(linecounter) = Xstr
                        YCoordinates(i)(linecounter) = Ystr
                        If XCoordinates(i)(linecounter) > Xmax Then
                            Xmax = XCoordinates(i)(linecounter)
                        ElseIf XCoordinates(i)(linecounter) < Xmin Then
                            Xmin = XCoordinates(i)(linecounter)
                        End If
                        If YCoordinates(i)(linecounter) > Ymax Then
                            Ymax = YCoordinates(i)(linecounter)
                        ElseIf YCoordinates(i)(linecounter) < Ymin Then
                            Ymin = YCoordinates(i)(linecounter)
                        End If
                        linecounter = linecounter + 1
                    End If
                Next
                Array.Resize(routeLines(i), linecounter)
            ElseIf profiles(i).StartsWith("p") Then
                roughLines(i) = profiles(i).Split("G")
                Dim linecounter As Integer = 0

                Dim Zcounter As Integer = 0
                For j As Integer = 0 To roughLines(i).Length - 1
                    If roughLines(i)(j).Contains("X") And roughLines(i)(j).Contains("Y") Then
                        Array.Resize(routeLines(i), roughLines(i).Length)
                        routeLines(i)(linecounter) = roughLines(i)(j) + " "
                        Dim Xindex As Integer = roughLines(i)(j).IndexOf("X")
                        Dim Yindex As Integer = roughLines(i)(j).IndexOf("Y")

                        Dim Xstr = CSng(routeLines(i)(linecounter).Substring(Xindex + 1, (routeLines(i)(linecounter).IndexOf(" ", Xindex) - Xindex - 1)))
                        Dim Ystr = CSng(routeLines(i)(linecounter).Substring(Yindex + 1, (routeLines(i)(linecounter).IndexOf(" ", Yindex) - Yindex - 1)))

                        Array.Resize(XCoordinates(i), linecounter + 1)
                        Array.Resize(YCoordinates(i), linecounter + 1)
                        XCoordinates(i)(linecounter) = Xstr
                        YCoordinates(i)(linecounter) = Ystr
                        If XCoordinates(i)(linecounter) > Xmax Then
                            Xmax = XCoordinates(i)(linecounter)
                        ElseIf XCoordinates(i)(linecounter) < Xmin Then
                            Xmin = XCoordinates(i)(linecounter)
                        End If
                        If YCoordinates(i)(linecounter) > Ymax Then
                            Ymax = YCoordinates(i)(linecounter)
                        ElseIf YCoordinates(i)(linecounter) < Ymin Then
                            Ymin = YCoordinates(i)(linecounter)
                        End If
                        linecounter = linecounter + 1
                    End If
                    If roughLines(i)(j).Contains("Z") And Zcounter < 2 Then
                        Zcounter = Zcounter + 1
                    ElseIf roughLines(i)(j).Contains("Z") Then
                        j = roughLines(i).Length - 1
                    End If
                Next
                Array.Resize(routeLines(i), linecounter)
            End If

        Next

        Return routeLines
    End Function

    Public Sub DrawLines()
        If fileLoaded = True Then
            Dim Xsize As Single = (Xmax - Xmin)
            Dim Ysize As Single = (Ymax - Ymin)
            Dim bImage As New Bitmap(1000, 600)
            Dim WidthRatio As Single = PictureBox1.Width / Xsize
            Dim HeightRatio As Single = PictureBox1.Height / Ysize
            Xoffset = Xmin - (Xsize * 0.05)
            Yoffset = Ymin - (Ysize * 0.05)
            If HeightRatio < WidthRatio Then
                ImageRatio = HeightRatio * 0.9
                Xoffset = Xmin - (Xsize * 0.05) * (WidthRatio / HeightRatio) - ((WidthRatio / HeightRatio) - 1) / 2 * Xsize
            Else
                ImageRatio = WidthRatio * 0.9
                Yoffset = Ymin - (Ysize * 0.05) * (HeightRatio / WidthRatio) - ((HeightRatio / WidthRatio) - 1) / 2 * Ysize
            End If

            Dim DesignImage = Graphics.FromImage(bImage)
            Dim pen As New Drawing.Pen(Color.RoyalBlue, TextBox1.Text * ImageRatio)
            Dim rectLength As Single = (TextBox1.Text * ImageRatio / 4) * 1.4142

            For i As Integer = 0 To XCoordinates.Length - 1
                For j As Integer = 0 To XCoordinates(i).Length - 2
                    Dim Xscaled = (XCoordinates(i)(j) - Xoffset) * ImageRatio
                    Dim Yscaled = (YCoordinates(i)(j) - Yoffset) * ImageRatio
                    Dim jNew As Integer = (j + 1) Mod (XCoordinates(i).Length)
                    DesignImage.DrawLine(pen, Xscaled, Yscaled, (XCoordinates(i)(jNew) - Xoffset) * ImageRatio, (YCoordinates(i)(jNew) - Yoffset) * ImageRatio)
                    DesignImage.FillEllipse(Brushes.RoyalBlue, Xscaled - rectLength, Yscaled - rectLength, rectLength * 2, rectLength * 2)
                Next
                Dim Xrect As Single = ((XCoordinates(i)(XCoordinates(i).Length - 1) - Xoffset) * ImageRatio) - rectLength
                Dim Yrect As Single = ((YCoordinates(i)(XCoordinates(i).Length - 1) - Yoffset) * ImageRatio) - rectLength
                DesignImage.FillEllipse(Brushes.RoyalBlue, Xrect, Yrect, rectLength * 2, rectLength * 2)
            Next
            If Xclick > 0 And Yclick > 0 Then
                Dim RedBrush As New SolidBrush(Color.Red)
                DesignImage.FillEllipse(RedBrush, Xclick - 15, Yclick - 37, 10, 10)
            End If

            PictureBox1.Image = bImage
        End If
    End Sub

    Private Sub PictureBox1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox1.Click
        Xclick = Math.Max(Math.Min(1000, MousePosition.X - PictureBox1.Location.X - Me.Location.X), 1)
        Yclick = Math.Max(Math.Min(600, MousePosition.Y - PictureBox1.Location.Y - Me.Location.Y), 1)
        DrawLines()
    End Sub

    Public Sub AddText()
        Dim Xpos As Single = Xclick / ImageRatio + Xoffset
        Dim Ypos As Single = Yclick / ImageRatio + Yoffset
        Dim tHeight As Single = TextBox3.Text
        Dim tWidth As Single = tHeight * 0.75
        Dim tSpace As Single = TextBox4.Text - 0.125
        Dim DrillSize As Single = TextBox1.Text
        Dim textString() As Char = TextBox2.Text
        Dim textCode As String = ""
        Dim textNumber As Integer = 0
        Dim depth As Single = -0.05
        For i As Integer = 0 To textString.Length - 1
            textString(i) = Char.ToLower(textString(i))
            Dim LetterCode As String = ""
            If textString(i) = "a" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + 0.375 * tWidth, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + 0.625 * tWidth, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" & Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + 2 * DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & "" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + 2 * DrillSize, 4) & " F60" & Environment.NewLine
            ElseIf textString(i) = "b" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" & Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & "" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & " F60" & Environment.NewLine
            ElseIf textString(i) = "c" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
            ElseIf textString(i) = "d" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + 2 * DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + 2 * DrillSize, 4) & Environment.NewLine
            ElseIf textString(i) = "e" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" & Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & "" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & " F60" & Environment.NewLine
            ElseIf textString(i) = "f" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" & Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & "" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & " F60" & Environment.NewLine
            ElseIf textString(i) = "g" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize + tWidth / 3, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
            ElseIf textString(i) = "h" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" & Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & "" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & " F60" & Environment.NewLine
            ElseIf textString(i) = "i" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" & Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth / 2, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & "" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth / 2, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & " F60" & Environment.NewLine
            ElseIf textString(i) = "j" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth - DrillSize / 2, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize / 2, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" & Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth / 2, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & "" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 6, 4) & Environment.NewLine
            ElseIf textString(i) = "k" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" & Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & "" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & " F60" & Environment.NewLine
            ElseIf textString(i) = "l" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
            ElseIf textString(i) = "m" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth / 2 + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 4 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize + tWidth, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                Xpos = Xpos + 2 * DrillSize
            ElseIf textString(i) = "n" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
            ElseIf textString(i) = "o" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
            ElseIf textString(i) = "p" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
            ElseIf textString(i) = "q" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" & Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth / 2, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + 2.5 * DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & "" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth + DrillSize / 8, 4) & " Y" & Math.Round(Ypos + DrillSize * 1.75, 4) & " F60" & Environment.NewLine
            ElseIf textString(i) = "r" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
            ElseIf textString(i) = "s" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize / 2, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
            ElseIf textString(i) = "t" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" & Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth / 2, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & "" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth / 2, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & " F60" & Environment.NewLine
            ElseIf textString(i) = "u" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
            ElseIf textString(i) = "v" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth / 2, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
            ElseIf textString(i) = "w" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth / 2 + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                Xpos = Xpos + 2 * DrillSize
            ElseIf textString(i) = "x" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
            ElseIf textString(i) = "y" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth / 2, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth / 2, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + Environment.NewLine & "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + tWidth / 2, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth / 2, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & " F60" & Environment.NewLine
            ElseIf textString(i) = "z" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos + DrillSize / 2, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize / 2, 4) & " Y" & Math.Round(Ypos - tHeight + DrillSize, 4) & " F60" & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + DrillSize / 2, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth - DrillSize / 2, 4) & " Y" & Math.Round(Ypos + DrillSize, 4) & Environment.NewLine
            ElseIf textString(i) = " " Then
                Xpos = Xpos - tWidth / 2
            ElseIf textString(i) = "-" Then
                LetterCode = LetterCode + "(text " & textNumber & ")" & Environment.NewLine
                textNumber = textNumber + 1
                LetterCode = LetterCode + "G0 Z0.5 F30" + Environment.NewLine
                LetterCode = LetterCode + "G0 X" & Math.Round(Xpos, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & Environment.NewLine
                LetterCode = LetterCode + "G1 Z" & depth & " F30" + Environment.NewLine
                LetterCode = LetterCode + "G1 X" & Math.Round(Xpos + tWidth / 2 - DrillSize, 4) & " Y" & Math.Round(Ypos - tHeight / 2 + DrillSize, 4) & " F60" & Environment.NewLine
                Xpos = Xpos - tWidth / 2
            Else
                MsgBox("The " & textString(i) & " character cannot be handled by the program at this time")
                Xpos = Xpos - tWidth - tSpace
            End If
            LetterCode = LetterCode & Environment.NewLine
            textCode = textCode & LetterCode
            Xpos = Xpos + tWidth + tSpace
        Next

        Dim tempCode As String = Gcode
        Dim indexI As Integer = tempCode.IndexOf("G0 X")
        Dim front As String = tempCode.Substring(0, indexI)

        tempCode = tempCode.Remove(0, indexI)
        Dim Bfront As String = front.Substring(0, front.LastIndexOf("("))
        Dim Efront As String = front.Substring(front.LastIndexOf(")") + 1)
        Dim GcodeFront As String = Bfront & Efront
        Dim name As String = front.Substring(front.LastIndexOf("("), front.LastIndexOf(")") - front.LastIndexOf("(") + 1)
        tempCode = tempCode.Insert(0, Environment.NewLine & name & Environment.NewLine & "G0 Z0.5" & Environment.NewLine)
        testCode = GcodeFront & textCode & tempCode
        RichTextBox1.Text = testCode

    End Sub


    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        If Xclick > 0 And Yclick > 0 And TextBox2.TextLength > 0 Then
            Try
                AddText()
                Dim CutLines()() As String = ComputeLines()
                DrawLines()
            Catch ex As Exception
                MsgBox("Could not add the text to the design")
            End Try
        ElseIf TextBox2.TextLength < 1 Then
            MsgBox("Please enter text into textbox to engrave into design")
        Else
            MsgBox("Please select a point on design to add text too")
        End If
    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        Gcode = testCode
        testCode = ""
        Dim CutLines()() As String = ComputeLines()
        DrawLines()
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        If Gcode <> OriginalCode Then
            If MsgBox("Are you sure you want to delete the text?", MsgBoxStyle.YesNo = MsgBoxStyle.DefaultButton2, "Delete Text from G-Code") = MsgBoxResult.Yes Then
                Gcode = OriginalCode
                testCode = ""
                RichTextBox1.Text = Gcode
                Dim CutLines()() As String = ComputeLines()
                DrawLines()
            End If
        End If
    End Sub

    Private Sub TextBox2_MouseClick(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles TextBox2.MouseClick
        If TextBox2.Text = "Text for Design" Then
            TextBox2.Text = ""
        End If
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        SaveFileDialog1.DefaultExt = ""
        SaveFileDialog1.ShowDialog()
        Try
            If SaveFileDialog1.CheckPathExists Then
                If testCode.Length > 2 Then
                    Gcode = testCode
                End If
                Dim SaveFile = SaveFileDialog1.FileName
                My.Computer.FileSystem.WriteAllText(SaveFileDialog1.FileName, Gcode, False, System.Text.Encoding.Default)
                Dim CutLines()() As String = ComputeLines()
                DrawLines()
            End If
        Catch ex As Exception
            MsgBox("Could not save G-Code to file " & SaveFileDialog1.FileName)
        End Try
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

    End Sub
End Class
