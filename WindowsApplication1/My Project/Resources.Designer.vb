﻿'------------------------------------------------------------------------------
' <auto-generated>
'     Este código fue generado por una herramienta.
'     Versión de runtime:4.0.30319.42000
'
'     Los cambios en este archivo podrían causar un comportamiento incorrecto y se perderán si
'     se vuelve a generar el código.
' </auto-generated>
'------------------------------------------------------------------------------

Option Strict On
Option Explicit On

Imports System

Namespace My.Resources
    
    'StronglyTypedResourceBuilder generó automáticamente esta clase
    'a través de una herramienta como ResGen o Visual Studio.
    'Para agregar o quitar un miembro, edite el archivo .ResX y, a continuación, vuelva a ejecutar ResGen
    'con la opción /str o recompile su proyecto de VS.
    '''<summary>
    '''  Clase de recurso fuertemente tipado, para buscar cadenas traducidas, etc.
    '''</summary>
    <Global.System.CodeDom.Compiler.GeneratedCodeAttribute("System.Resources.Tools.StronglyTypedResourceBuilder", "4.0.0.0"),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Runtime.CompilerServices.CompilerGeneratedAttribute(),  _
     Global.Microsoft.VisualBasic.HideModuleNameAttribute()>  _
    Friend Module Resources
        
        Private resourceMan As Global.System.Resources.ResourceManager
        
        Private resourceCulture As Global.System.Globalization.CultureInfo
        
        '''<summary>
        '''  Devuelve la instancia de ResourceManager almacenada en caché utilizada por esta clase.
        '''</summary>
        <Global.System.ComponentModel.EditorBrowsableAttribute(Global.System.ComponentModel.EditorBrowsableState.Advanced)>  _
        Friend ReadOnly Property ResourceManager() As Global.System.Resources.ResourceManager
            Get
                If Object.ReferenceEquals(resourceMan, Nothing) Then
                    Dim temp As Global.System.Resources.ResourceManager = New Global.System.Resources.ResourceManager("WindowsApplication1.Resources", GetType(Resources).Assembly)
                    resourceMan = temp
                End If
                Return resourceMan
            End Get
        End Property
        
        '''<summary>
        '''  Reemplaza la propiedad CurrentUICulture del subproceso actual para todas las
        '''  búsquedas de recursos mediante esta clase de recurso fuertemente tipado.
        '''</summary>
        <Global.System.ComponentModel.EditorBrowsableAttribute(Global.System.ComponentModel.EditorBrowsableState.Advanced)>  _
        Friend Property Culture() As Global.System.Globalization.CultureInfo
            Get
                Return resourceCulture
            End Get
            Set
                resourceCulture = value
            End Set
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property alfil_blanco() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("alfil_blanco", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property alfil_negro() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("alfil_negro", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property AMARILLO() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("AMARILLO", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property caballo_blanco() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("caballo_blanco", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property caballo_invertido_blanco() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("caballo_invertido_blanco", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property caballo_invertido_negro() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("caballo_invertido_negro", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property caballo_negro() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("caballo_negro", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property disponible() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("disponible", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca una cadena traducida similar a img/blancas/peon.png.
        '''</summary>
        Friend ReadOnly Property peon() As String
            Get
                Return ResourceManager.GetString("peon", resourceCulture)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property peon_blanco() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("peon_blanco", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property peon_negro() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("peon_negro", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property reina_blanco() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("reina_blanco", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property reina_negro() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("reina_negro", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property rey_blanco() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("rey_blanco", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property ROJO() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("ROJO", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property tablero() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("tablero", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property torre_blanco() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("torre_blanco", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property torre_nergo() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("torre_nergo", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Busca un recurso adaptado de tipo System.Drawing.Bitmap.
        '''</summary>
        Friend ReadOnly Property VERDE() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("VERDE", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
    End Module
End Namespace