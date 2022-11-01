' ES POR ACA???? timer???
'If peonBlanco4.Bounds.IntersectsWith(Bounds) Then
'   MsgBox("CHOQUE")
'End If

Public Class Form1
    Public Function estadoBlancos(ByVal estado)
        peonBlanco1.Enabled = estado
        peonBlanco2.Enabled = estado
        peonBlanco3.Enabled = estado
        peonBlanco4.Enabled = estado
        peonBlanco5.Enabled = estado
        peonBlanco6.Enabled = estado
        peonBlanco7.Enabled = estado
        peonBlanco8.Enabled = estado
        torreBlanco1.Enabled = estado
        caballoBlanco1.Enabled = estado
        alfilBlanco1.Enabled = estado
        reinaBlanco.Enabled = estado
        reyBlanco.Enabled = estado
        alfilBlanco2.Enabled = estado
        caballoBlanco2.Enabled = estado
        torreBlanco2.Enabled = estado
    End Function

    Public Function estadoNegros(ByVal estado)
        peonNegro1.Enabled = estado
        peonNegro2.Enabled = estado
        peonNegro3.Enabled = estado
        peonNegro4.Enabled = estado
        peonNegro5.Enabled = estado
        peonNegro6.Enabled = estado
        peonNegro7.Enabled = estado
        peonNegro8.Enabled = estado
        torreNegro1.Enabled = estado
        caballoNegro1.Enabled = estado
        alfilNegro1.Enabled = estado
        reinaNegro.Enabled = estado
        reyNegro.Enabled = estado
        alfilNegro2.Enabled = estado
        caballoNegro2.Enabled = estado
        torreNegro2.Enabled = estado
    End Function

    Public Function restablecerDispos()
        dispo1.Visible = False
        dispo1.Location = New Point(variablesGlobales.defaultDispoLocationX, variablesGlobales.defaultDispoLocationY)
        dispo2.Visible = False
        dispo2.Location = New Point(variablesGlobales.defaultDispoLocationX, variablesGlobales.defaultDispoLocationY)
        dispo3.Visible = False
        dispo3.Location = New Point(variablesGlobales.defaultDispoLocationX, variablesGlobales.defaultDispoLocationY)
        dispo4.Visible = False
        dispo4.Location = New Point(variablesGlobales.defaultDispoLocationX, variablesGlobales.defaultDispoLocationY)
        dispo5.Visible = False
        dispo5.Location = New Point(variablesGlobales.defaultDispoLocationX, variablesGlobales.defaultDispoLocationY)
        dispo6.Visible = False
        dispo6.Location = New Point(variablesGlobales.defaultDispoLocationX, variablesGlobales.defaultDispoLocationY)
        dispo7.Visible = False
        dispo7.Location = New Point(variablesGlobales.defaultDispoLocationX, variablesGlobales.defaultDispoLocationY)
    End Function

    Public Class variablesGlobales
        Public Shared piezaSeleccionada As String = ""
        Public Shared distancia As Integer = 85 'Cambiar por 80 (probablemente)
        Public Shared defaultDispoLocationX As Integer = 733
        Public Shared defaultDispoLocationY As Integer = 570

        'Posiciones iniciales de peones (para saber si es posible mover 2 casillas en un turno)
        Public Shared peonBlanco1X As Integer = 34
        Public Shared peonBlanco1Y As Integer = 532
        '
        Public Shared peonBlanco2X As Integer = 117
        Public Shared peonBlanco2Y As Integer = 532
        '
        Public Shared peonBlanco3X As Integer = 200
        Public Shared peonBlanco3Y As Integer = 532
        '
        Public Shared peonBlanco4X As Integer = 283
        Public Shared peonBlanco4Y As Integer = 532
        '
        Public Shared peonBlanco5X As Integer = 367
        Public Shared peonBlanco5Y As Integer = 532
        '
        Public Shared peonBlanco6X As Integer = 449
        Public Shared peonBlanco6Y As Integer = 532
        '
        Public Shared peonBlanco7X As Integer = 531
        Public Shared peonBlanco7Y As Integer = 532
        '
        Public Shared peonBlanco8X As Integer = 615
        Public Shared peonBlanco8Y As Integer = 532

        'NEGROS
        Public Shared peonNegro1X As Integer = 34
        Public Shared peonNegro1Y As Integer = 117
        '
        Public Shared peonNegro2X As Integer = 117
        Public Shared peonNegro2Y As Integer = 117
        '
        Public Shared peonNegro3X As Integer = 0
        Public Shared peonNegro3Y As Integer = 0
        '
        Public Shared peonNegro4X As Integer = 0
        Public Shared peonNegro4Y As Integer = 0
        '
        Public Shared peonNegro5X As Integer = 0
        Public Shared peonNegro5Y As Integer = 0
        '
        Public Shared peonNegro6X As Integer = 0
        Public Shared peonNegro6Y As Integer = 0
        '
        Public Shared peonNegro7X As Integer = 0
        Public Shared peonNegro7Y As Integer = 0
        '
        Public Shared peonNegro8X As Integer = 0
        Public Shared peonNegro8Y As Integer = 0

    End Class

    Public Function mover(ByVal pieza, ByVal x, ByVal y)
        Label3.Text = "Estoy moviendo: " + pieza.ToString
        If pieza = "peonBlanco1" Then
            pieza = peonBlanco1
        ElseIf pieza = "peonBlanco2" Then
            pieza = peonBlanco2
        ElseIf pieza = "peonBlanco3" Then
            pieza = peonBlanco3
        ElseIf pieza = "peonBlanco4" Then
            pieza = peonBlanco4
        ElseIf pieza = "peonBlanco5" Then
            pieza = peonBlanco5
        ElseIf pieza = "peonBlanco6" Then
            pieza = peonBlanco6
        ElseIf pieza = "peonBlanco7" Then
            pieza = peonBlanco7
        ElseIf pieza = "peonBlanco8" Then
            pieza = peonBlanco8
        ElseIf pieza = "torreBlanco1" Then
            pieza = torreBlanco1
        ElseIf pieza = "caballoBlanco1" Then
            pieza = caballoBlanco1
        ElseIf pieza = "alfilBlanco1" Then
            pieza = alfilBlanco1
        ElseIf pieza = "reinaBlanco" Then
            pieza = reinaBlanco
        ElseIf pieza = "reyBlanco" Then
            pieza = reyBlanco
        ElseIf pieza = "alfilBlanco2" Then
            pieza = alfilBlanco2
        ElseIf pieza = "caballoBlanco2" Then
            pieza = caballoBlanco2
        ElseIf pieza = "torreBlanco2" Then
            pieza = torreBlanco2
            ' PIEZAS NEGRAS
        ElseIf pieza = "peonNegro1" Then
            pieza = peonNegro1
        ElseIf pieza = "peonNegro2" Then
            pieza = peonNegro2
        ElseIf pieza = "peonNegro3" Then
            pieza = peonNegro3
        ElseIf pieza = "peonNegro4" Then
            pieza = peonNegro4
        ElseIf pieza = "peonNegro5" Then
            pieza = peonNegro5
        ElseIf pieza = "peonNegro6" Then
            pieza = peonNegro6
        ElseIf pieza = "peonNegro7" Then
            pieza = peonNegro7
        ElseIf pieza = "peonNegro8" Then
            pieza = peonNegro8
        ElseIf pieza = "torreNegro1" Then
            pieza = torreNegro1
        ElseIf pieza = "caballoNegro1" Then
            pieza = caballoNegro1
        ElseIf pieza = "alfilNegro1" Then
            pieza = alfilNegro1
        ElseIf pieza = "reinaNegro" Then
            pieza = reinaNegro
        ElseIf pieza = "reyNegro" Then
            pieza = reyNegro
        ElseIf pieza = "alfilNegro2" Then
            pieza = alfilNegro2
        ElseIf pieza = "caballoNegro2" Then
            pieza = caballoNegro2
        ElseIf pieza = "torreNegro2" Then
            pieza = torreNegro2
        End If

        pieza.location = New Point(x, y)
    End Function

    Public Function moverPeon(ByVal pieza, ByVal movimiento, ByVal tipo)
        Dim x As Integer = pieza.location.x
        Dim y As Integer
        If tipo = "blanco" Then
            y = pieza.location.y - (variablesGlobales.distancia * movimiento)
        ElseIf tipo = "negro" Then
            y = pieza.location.y + (variablesGlobales.distancia * movimiento)
        End If
        pieza.location = New Point(x, y)
        Label2.Text = "Nueva posicion: " + x.ToString + " " + y.ToString
    End Function

    'Dispos
    Private Sub dispo1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles dispo1.Click
        Dim x As Integer = dispo1.Location.X
        Dim y As Integer = dispo1.Location.Y
        mover(variablesGlobales.piezaSeleccionada, x, y) 'Llama a la funcion que mueve la pieza seleccionada al lugar de dispo
        variablesGlobales.piezaSeleccionada = "default" 'Limpia la variable para poder reutilizarla

        'Se restablecen los valores predeteriminados de dispo para que no se vea
        restablecerDispos()
    End Sub

    Private Sub dispo2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles dispo2.Click
        Dim x As Integer = dispo2.Location.X
        Dim y As Integer = dispo2.Location.Y
        mover(variablesGlobales.piezaSeleccionada, x, y)
        variablesGlobales.piezaSeleccionada = "default"

        'Se restablecen los valores predeteriminados de dispo para que no se vea
        restablecerDispos()
    End Sub

    Private Sub dispo3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles dispo3.Click
        Dim x As Integer = dispo3.Location.X
        Dim y As Integer = dispo3.Location.Y
        mover(variablesGlobales.piezaSeleccionada, x, y)
        variablesGlobales.piezaSeleccionada = "default"

        'Se restablecen los valores predeteriminados de dispo para que no se vea
        restablecerDispos()
    End Sub

    Private Sub dispo4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles dispo4.Click
        Dim x As Integer = dispo4.Location.X
        Dim y As Integer = dispo4.Location.Y
        mover(variablesGlobales.piezaSeleccionada, x, y)
        variablesGlobales.piezaSeleccionada = "default"

        'Se restablecen los valores predeteriminados de dispo para que no se vea
        restablecerDispos()
    End Sub

    Private Sub dispo5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles dispo5.Click
        Dim x As Integer = dispo5.Location.X
        Dim y As Integer = dispo5.Location.Y
        mover(variablesGlobales.piezaSeleccionada, x, y)
        variablesGlobales.piezaSeleccionada = "default"

        'Se restablecen los valores predeteriminados de dispo para que no se vea
        restablecerDispos()
    End Sub

    Private Sub dispo6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles dispo6.Click
        Dim x As Integer = dispo6.Location.X
        Dim y As Integer = dispo6.Location.Y
        mover(variablesGlobales.piezaSeleccionada, x, y)
        variablesGlobales.piezaSeleccionada = "default"

        'Se restablecen los valores predeteriminados de dispo para que no se vea
        restablecerDispos()
    End Sub

    Private Sub dispo7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles dispo7.Click
        Dim x As Integer = dispo7.Location.X
        Dim y As Integer = dispo7.Location.Y
        mover(variablesGlobales.piezaSeleccionada, x, y)
        variablesGlobales.piezaSeleccionada = "default"

        'Se restablecen los valores predeteriminados de dispo para que no se vea
        restablecerDispos()
    End Sub

    'Peones blancos
    Private Sub peonBlanco1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles peonBlanco1.Click
        If peonBlanco1.Location.X = variablesGlobales.peonBlanco1X And peonBlanco1.Location.Y = variablesGlobales.peonBlanco1Y Then
            'Pone los dispos necesarios en la pos del peon
            dispo1.Location = New Point(peonBlanco1.Location.X, peonBlanco1.Location.Y)
            dispo2.Location = New Point(peonBlanco1.Location.X, peonBlanco1.Location.Y)
            'Los hace visibles para que los vea el jugador
            dispo1.Visible = True
            dispo2.Visible = True
            'Llama a la funcion que coloca los dispos en las casillas superiores
            moverPeon(dispo1, 1, "blanco") '(Objeto, cantidad de movimientos, tipo de pieza)
            moverPeon(dispo2, 2, "blanco")
            'Deshabilita todas las piezas blancas y le da lugar a las negras (cambio de turno)
            estadoBlancos(False)
            estadoNegros(True)
        Else
            dispo1.Location = New Point(peonBlanco1.Location.X, peonBlanco1.Location.Y)
            dispo1.Visible = True
            moverPeon(dispo1, 1, "blanco")
            estadoBlancos(False)
            estadoNegros(True)
        End If

        variablesGlobales.piezaSeleccionada = "peonBlanco1" 'Guarda en una variable global la pieza con la que se esta trabajando
    End Sub

    Private Sub peonBlanco2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles peonBlanco2.Click
        If peonBlanco2.Location.X = variablesGlobales.peonBlanco2X And peonBlanco2.Location.Y = variablesGlobales.peonBlanco2Y Then
            dispo1.Location = New Point(peonBlanco2.Location.X, peonBlanco2.Location.Y)
            dispo2.Location = New Point(peonBlanco2.Location.X, peonBlanco2.Location.Y)
            dispo1.Visible = True
            dispo2.Visible = True
            moverPeon(dispo1, 1, "blanco")
            moverPeon(dispo2, 2, "blanco")

            estadoBlancos(False)
            estadoNegros(True)
        Else
            dispo1.Location = New Point(peonBlanco2.Location.X, peonBlanco2.Location.Y)
            dispo1.Visible = True
            moverPeon(dispo1, 1, "blanco")
            estadoBlancos(False)
            estadoNegros(True)
        End If

        variablesGlobales.piezaSeleccionada = "peonBlanco2"
    End Sub

    Private Sub peonBlanco3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles peonBlanco3.Click
        If peonBlanco3.Location.X = variablesGlobales.peonBlanco3X And peonBlanco3.Location.Y = variablesGlobales.peonBlanco3Y Then
            dispo1.Location = New Point(peonBlanco3.Location.X, peonBlanco3.Location.Y)
            dispo2.Location = New Point(peonBlanco3.Location.X, peonBlanco3.Location.Y)
            dispo1.Visible = True
            dispo2.Visible = True
            moverPeon(dispo1, 1, "blanco")
            moverPeon(dispo2, 2, "blanco")

            estadoBlancos(False)
            estadoNegros(True)
        Else
            dispo1.Location = New Point(peonBlanco3.Location.X, peonBlanco3.Location.Y)
            dispo1.Visible = True
            moverPeon(dispo1, 1, "blanco")
            estadoBlancos(False)
            estadoNegros(True)
        End If

        variablesGlobales.piezaSeleccionada = "peonBlanco3"
    End Sub

    Private Sub peonBlanco4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles peonBlanco4.Click
        If peonBlanco4.Location.X = variablesGlobales.peonBlanco4X And peonBlanco4.Location.Y = variablesGlobales.peonBlanco4Y Then
            dispo1.Location = New Point(peonBlanco4.Location.X, peonBlanco4.Location.Y)
            dispo2.Location = New Point(peonBlanco4.Location.X, peonBlanco4.Location.Y)
            dispo1.Visible = True
            dispo2.Visible = True
            moverPeon(dispo1, 1, "blanco")
            moverPeon(dispo2, 2, "blanco")

            estadoBlancos(False)
            estadoNegros(True)
        Else
            dispo1.Location = New Point(peonBlanco4.Location.X, peonBlanco4.Location.Y)
            dispo1.Visible = True
            moverPeon(dispo1, 1, "blanco")
            estadoBlancos(False)
            estadoNegros(True)
        End If

        variablesGlobales.piezaSeleccionada = "peonBlanco4"
    End Sub

    Private Sub peonBlanco5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles peonBlanco5.Click
        If peonBlanco5.Location.X = variablesGlobales.peonBlanco5X And peonBlanco5.Location.Y = variablesGlobales.peonBlanco5Y Then
            dispo1.Location = New Point(peonBlanco5.Location.X, peonBlanco5.Location.Y)
            dispo2.Location = New Point(peonBlanco5.Location.X, peonBlanco5.Location.Y)
            dispo1.Visible = True
            dispo2.Visible = True
            moverPeon(dispo1, 1, "blanco")
            moverPeon(dispo2, 2, "blanco")

            estadoBlancos(False)
            estadoNegros(True)
        Else
            dispo1.Location = New Point(peonBlanco5.Location.X, peonBlanco5.Location.Y)
            dispo1.Visible = True
            moverPeon(dispo1, 1, "blanco")
            estadoBlancos(False)
            estadoNegros(True)
        End If

        variablesGlobales.piezaSeleccionada = "peonBlanco5"
    End Sub

    Private Sub peonBlanco6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles peonBlanco6.Click
        If peonBlanco6.Location.X = variablesGlobales.peonBlanco6X And peonBlanco6.Location.Y = variablesGlobales.peonBlanco6Y Then
            dispo1.Location = New Point(peonBlanco6.Location.X, peonBlanco6.Location.Y)
            dispo2.Location = New Point(peonBlanco6.Location.X, peonBlanco6.Location.Y)
            dispo1.Visible = True
            dispo2.Visible = True
            moverPeon(dispo1, 1, "blanco")
            moverPeon(dispo2, 2, "blanco")

            estadoBlancos(False)
            estadoNegros(True)
        Else
            dispo1.Location = New Point(peonBlanco6.Location.X, peonBlanco6.Location.Y)
            dispo1.Visible = True
            moverPeon(dispo1, 1, "blanco")
            estadoBlancos(False)
            estadoNegros(True)
        End If

        variablesGlobales.piezaSeleccionada = "peonBlanco6"
    End Sub

    Private Sub peonBlanco7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles peonBlanco7.Click
        If peonBlanco7.Location.X = variablesGlobales.peonBlanco7X And peonBlanco7.Location.Y = variablesGlobales.peonBlanco7Y Then
            dispo1.Location = New Point(peonBlanco7.Location.X, peonBlanco7.Location.Y)
            dispo2.Location = New Point(peonBlanco7.Location.X, peonBlanco7.Location.Y)
            dispo1.Visible = True
            dispo2.Visible = True
            moverPeon(dispo1, 1, "blanco")
            moverPeon(dispo2, 2, "blanco")

            estadoBlancos(False)
            estadoNegros(True)
        Else
            dispo1.Location = New Point(peonBlanco7.Location.X, peonBlanco7.Location.Y)
            dispo1.Visible = True
            moverPeon(dispo1, 1, "blanco")
            estadoBlancos(False)
            estadoNegros(True)
        End If

        variablesGlobales.piezaSeleccionada = "peonBlanco7"
    End Sub

    Private Sub peonBlanco8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles peonBlanco8.Click
        If peonBlanco8.Location.X = variablesGlobales.peonBlanco8X And peonBlanco8.Location.Y = variablesGlobales.peonBlanco8Y Then
            dispo1.Location = New Point(peonBlanco8.Location.X, peonBlanco8.Location.Y)
            dispo2.Location = New Point(peonBlanco8.Location.X, peonBlanco8.Location.Y)
            dispo1.Visible = True
            dispo2.Visible = True
            moverPeon(dispo1, 1, "blanco")
            moverPeon(dispo2, 2, "blanco")

            estadoBlancos(False)
            estadoNegros(True)
        Else
            dispo1.Location = New Point(peonBlanco8.Location.X, peonBlanco8.Location.Y)
            dispo1.Visible = True
            moverPeon(dispo1, 1, "blanco")
            estadoBlancos(False)
            estadoNegros(True)
        End If

        variablesGlobales.piezaSeleccionada = "peonBlanco8"
    End Sub

    'Peones negros
    Private Sub peonNegro1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles peonNegro1.Click
        If peonNegro1.Location.X = variablesGlobales.peonNegro1X And peonNegro1.Location.Y = variablesGlobales.peonNegro1Y Then
            dispo1.Location = New Point(peonNegro1.Location.X, peonNegro1.Location.Y)
            dispo2.Location = New Point(peonNegro1.Location.X, peonNegro1.Location.Y)
            dispo1.Visible = True
            dispo2.Visible = True
            moverPeon(dispo1, 1, "negro")
            moverPeon(dispo2, 2, "negro")
            estadoNegros(False)
            estadoBlancos(True)
        Else
            dispo1.Location = New Point(peonNegro1.Location.X, peonNegro1.Location.Y)
            dispo1.Visible = True
            moverPeon(dispo1, 1, "negro")
            estadoNegros(False)
            estadoBlancos(True)
        End If
        variablesGlobales.piezaSeleccionada = "peonNegro1"
    End Sub

    Private Sub peonNegro2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles peonNegro2.Click
        If peonNegro2.Location.X = variablesGlobales.peonNegro2X And peonNegro2.Location.Y = variablesGlobales.peonNegro2Y Then
            dispo1.Location = New Point(peonNegro2.Location.X, peonNegro2.Location.Y)
            dispo2.Location = New Point(peonNegro2.Location.X, peonNegro2.Location.Y)
            dispo1.Visible = True
            dispo2.Visible = True
            moverPeon(dispo1, 1, "negro")
            moverPeon(dispo2, 2, "negro")
            estadoNegros(False)
            estadoBlancos(True)
        Else
            dispo1.Location = New Point(peonNegro2.Location.X, peonNegro2.Location.Y)
            dispo1.Visible = True
            moverPeon(dispo1, 1, "negro")
            estadoNegros(False)
            estadoBlancos(True)
        End If
        variablesGlobales.piezaSeleccionada = "peonNegro2"
    End Sub

    'Torres blancas
    Public Function moverTorre(ByVal pieza, ByVal movimiento)
        Dim x As Integer = pieza.location.x
        Dim y As Integer = pieza.location.y - variablesGlobales.distancia * movimiento
        pieza.location = New Point(x, y)
        Label2.Text = "Nueva posicion: " + x.ToString + " " + y.ToString
    End Function

    Private Sub torreBlanco1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles torreBlanco1.Click
        dispo1.Location = New Point(torreBlanco1.Location.X, torreBlanco1.Location.Y)
        dispo2.Location = New Point(torreBlanco1.Location.X, torreBlanco1.Location.Y)
        dispo3.Location = New Point(torreBlanco1.Location.X, torreBlanco1.Location.Y)
        dispo4.Location = New Point(torreBlanco1.Location.X, torreBlanco1.Location.Y)
        dispo5.Location = New Point(torreBlanco1.Location.X, torreBlanco1.Location.Y)
        dispo6.Location = New Point(torreBlanco1.Location.X, torreBlanco1.Location.Y)
        dispo7.Location = New Point(torreBlanco1.Location.X, torreBlanco1.Location.Y)
        dispo1.Visible = True
        dispo2.Visible = True
        dispo3.Visible = True
        dispo4.Visible = True
        dispo5.Visible = True
        dispo6.Visible = True
        dispo7.Visible = True
        moverTorre(dispo1, 1)
        moverTorre(dispo2, 2)
        moverTorre(dispo3, 3)
        moverTorre(dispo4, 4)
        moverTorre(dispo5, 5)
        moverTorre(dispo6, 6)
        moverTorre(dispo7, 7)
        estadoBlancos(False)

        variablesGlobales.piezaSeleccionada = "torreBlanco1"
    End Sub

    Private Sub torreBlanco2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles torreBlanco2.Click
        dispo1.Location = New Point(torreBlanco2.Location.X, torreBlanco2.Location.Y)
        dispo2.Location = New Point(torreBlanco2.Location.X, torreBlanco2.Location.Y)
        dispo3.Location = New Point(torreBlanco2.Location.X, torreBlanco2.Location.Y)
        dispo4.Location = New Point(torreBlanco2.Location.X, torreBlanco2.Location.Y)
        dispo5.Location = New Point(torreBlanco2.Location.X, torreBlanco2.Location.Y)
        dispo6.Location = New Point(torreBlanco2.Location.X, torreBlanco2.Location.Y)
        dispo7.Location = New Point(torreBlanco2.Location.X, torreBlanco2.Location.Y)
        dispo1.Visible = True
        dispo2.Visible = True
        dispo3.Visible = True
        dispo4.Visible = True
        dispo5.Visible = True
        dispo6.Visible = True
        dispo7.Visible = True
        moverTorre(dispo1, 1)
        moverTorre(dispo2, 2)
        moverTorre(dispo3, 3)
        moverTorre(dispo4, 4)
        moverTorre(dispo5, 5)
        moverTorre(dispo6, 6)
        moverTorre(dispo7, 7)
        estadoBlancos(False)

        variablesGlobales.piezaSeleccionada = "torreBlanco2"
    End Sub

    ' POR ACAAAAAAA VER FUNCION ALFIL, MOV, ETC
    Public Function moverAlfil(ByVal pieza, ByVal movimiento)
        Dim x As Integer = pieza.location.x
        Dim y As Integer = pieza.location.y - variablesGlobales.distancia * movimiento
        pieza.location = New Point(x, y)
        Label2.Text = "Nueva posicion: " + x.ToString + " " + y.ToString
    End Function

    Private Sub alfilBlanco1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles alfilBlanco1.Click
        dispo1.Location = New Point(alfilBlanco1.Location.X, alfilBlanco1.Location.Y)
        dispo2.Location = New Point(alfilBlanco1.Location.X, alfilBlanco1.Location.Y)
        dispo3.Location = New Point(alfilBlanco1.Location.X, alfilBlanco1.Location.Y)
        dispo4.Location = New Point(alfilBlanco1.Location.X, alfilBlanco1.Location.Y)
        dispo5.Location = New Point(alfilBlanco1.Location.X, alfilBlanco1.Location.Y)
        dispo6.Location = New Point(alfilBlanco1.Location.X, alfilBlanco1.Location.Y)
        dispo7.Location = New Point(alfilBlanco1.Location.X, alfilBlanco1.Location.Y)
        dispo1.Visible = True
        dispo2.Visible = True
        dispo3.Visible = True
        dispo4.Visible = True
        dispo5.Visible = True
        dispo6.Visible = True
        dispo7.Visible = True
    End Sub
End Class
