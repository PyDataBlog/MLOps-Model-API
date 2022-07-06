Imports NeuralNetwork.Activation
Imports System.Xml

Module NetworkOperation
    Public Sub loadTrainingData()
        'load testing inputs
        Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(form_main.tb_input.Text)
            MyReader.TextFieldType = FileIO.FieldType.Delimited
            MyReader.SetDelimiters(",")

            ReDim TrainingMode.inputData(TrainingMode.numInputLines - 1, TrainingMode.numInputs - 1)
            Dim i As Integer = 0
            Dim currentRow As String()
            While Not MyReader.EndOfData
                Try
                    currentRow = MyReader.ReadFields()

                    Dim currentField As String
                    Dim j As Integer = 0
                    For Each currentField In currentRow
                        TrainingMode.inputData(i, j) = currentField
                        j += 1
                    Next

                    i += 1
                Catch ex As Microsoft.VisualBasic.FileIO.MalformedLineException
                    MsgBox("Line " & ex.Message & "is not valid and will be skipped.")
                End Try
            End While
        End Using

        'load expected outputs
        Using MyReader As New Microsoft.VisualBasic.FileIO.TextFieldParser(form_main.tb_output.Text)
            MyReader.TextFieldType = FileIO.FieldType.Delimited
            MyReader.SetDelimiters(",")

            ReDim TrainingMode.expectedOutputs(TrainingMode.numOutputLines - 1, TrainingMode.expectedOutputsPerLine - 1)
            Dim i As Integer = 0
            Dim currentRow As String()
            While Not MyReader.EndOfData
                Try
                    currentRow = MyReader.ReadFields()

                    Dim currentField As String
                    Dim j As Integer = 0
                    For Each currentField In currentRow
                        TrainingMode.expectedOutputs(i, j) = currentField
                        j += 1
                    Next

                    i += 1
                Catch ex As Microsoft.VisualBasic.FileIO.MalformedLineException
                    MsgBox("Line " & ex.Message & "is not valid and will be skipped.")
                End Try
            End While
        End Using
    End Sub

    Private Sub layerCalculate(network As BackpropagationNetwork, layerIndex As Integer)
        Dim currentLayer As Layer = network.Layers(layerIndex)
        Dim prevLayer As Layer = network.Layers(layerIndex - 1)

        Dim sum(currentLayer.NeuronCount - 1) As Double

        'Pre-populate sums with bias values
        For i = 0 To currentLayer.NeuronCount - 1
            sum(i) = currentLayer.Bias(i)
        Next

        'calculate each node's sum
        For currentNeuronInCurrentLayer = 0 To currentLayer.NeuronCount - 1
            For currentNeuronInPrevLayer = 0 To prevLayer.NeuronCount - 1
                sum(currentNeuronInCurrentLayer) += prevLayer.Outputs(currentNeuronInPrevLayer) * prevLayer.Weights(currentNeuronInPrevLayer, currentNeuronInCurrentLayer)
            Next
        Next

        currentLayer.Inputs = sum

        'calcute the function value (output) using an activation function
        Dim functionValue(currentLayer.NeuronCount - 1) As Double
        For i = 0 To currentLayer.NeuronCount - 1
            functionValue(i) = ActivationFunctions.Evaluate(currentLayer.ActivationFunction, sum(i))
        Next

        currentLayer.Outputs = functionValue
    End Sub

    Public Sub networkCalculate(networkToCalculate As BackpropagationNetwork)
        Dim i As Integer = 0
        For Each layer In networkToCalculate.Layers
            If layer.LayerType = ILayer.LayerType_.Input Then
                'do nothing
            Else
                layerCalculate(networkToCalculate, i)
            End If

            i += 1
        Next
    End Sub

    Public Function trainNetwork(ByRef network As BackpropagationNetwork, learningRate As Double, momentum As Double) As Double
        Dim exampleError(numInputLines - 1) As Double
        For ex = 0 To numInputLines - 1 'for each example

            'run the network once to get output values
            network.Layers(0).Outputs = Util.Array.GetRow(ex, inputData)
            networkCalculate(network)

            'do error calculation
            For l = network.LayerCount - 1 To 1 Step -1 'for each layer (exept input layer), calculate the error, starting from the back
                Dim currentLayer As Layer = network.Layers(l)
                Dim prevLayer As Layer = network.Layers(l - 1)

                If network.Layers(l).LayerType = ILayer.LayerType_.Output Then

                    For k = 0 To network.LastLayer.NeuronCount - 1 'for each output neuron
                        Dim diff As Double
                        diff = network.LastLayer.Outputs(k) - expectedOutputs(ex, k)

                        exampleError(ex) += diff ^ 2

                        Dim delta_k As Double
                        delta_k = diff * ActivationFunctions.EvaluateDerivative(network.Layers(l).ActivationFunction, network.Layers(l).Inputs(k)) 'the formula for delta_k

                        network.Layers(l).Deltas(k) = delta_k
                    Next

                Else 'hidden layer

                    For i = 1 To network.Layers(l).NeuronCount - 1 'for each neuron in the current layer

                        'for each neuron in the next layer (the layer nearer to the output), calculate the delta_j_tempSum (for use in calculating delta_j)
                        Dim delta_j_tempSum As Double = 0
                        For j = 0 To network.Layers(l + 1).NeuronCount - 1
                            delta_j_tempSum += network.Layers(l + 1).Deltas(j) * network.Layers(l).Weights(i, j)
                        Next

                        Dim delta_j As Double
                        delta_j = ActivationFunctions.EvaluateDerivative(network.Layers(l).ActivationFunction, network.Layers(l).Inputs(i)) * delta_j_tempSum 'the formula for delta_j

                        network.Layers(l).Deltas(i) = delta_j
                    Next

                End If

            Next


            'update the weights
            For layer = 2 To network.LayerCount - 1 'for each layer (except input layer)
                Dim currentLayer As Layer = network.Layers(layer)
                Dim prevLayer As Layer = network.Layers(layer - 1)

                For i = 0 To prevLayer.NeuronCount - 1 'for each neuron in the previous layer

                    For j = 0 To currentLayer.NeuronCount - 1 'each neuron in the current layer

                        prevLayer.WeightDeltas(i, j) = learningRate * currentLayer.Deltas(j) * prevLayer.Outputs(i)

                        prevLayer.Weights(i, j) -= prevLayer.WeightDeltas(i, j) + momentum * prevLayer.PreviousWeightDeltas(i, j)

                        prevLayer.PreviousWeightDeltas(i, j) = prevLayer.WeightDeltas(i, j)
                    Next
                Next
            Next

            If form_main.chk_updateBias.Checked Then
                For layer = 1 To network.LayerCount - 1 'for each layer (except input layer)
                    For i = 0 To network.Layers(layer).NeuronCount - 1 'each neuron in the current layer

                        network.Layers(layer).BiasDeltas(i) = learningRate * network.Layers(layer).Deltas(i)

                        network.Layers(layer).Bias(i) -= network.Layers(layer).BiasDeltas(i) + momentum * network.Layers(layer).PreviousBiasDeltas(i)

                        network.Layers(layer).PreviousBiasDeltas(i) = network.Layers(layer).BiasDeltas(i)
                    Next
                Next
            End If
        Next

        Dim exampleErrorSum As Double
        For i = 0 To numInputLines - 1
            exampleErrorSum += exampleError(i)
        Next

        Dim MSE As Double = exampleErrorSum / numInputLines
        Dim RMSE As Double = Math.Sqrt(MSE)

        Return RMSE
    End Function

    Public Sub Save(network As BackpropagationNetwork, FilePath As String)
        Dim settings As New XmlWriterSettings
        settings.Indent = True

        Dim writer As XmlWriter = XmlWriter.Create(FilePath, settings)

        With writer
            .WriteComment("Backpropagation Neural Network, created " & Date.Now().ToString)
            .WriteStartElement("Network")

            .WriteStartElement("Network-Parameters")

            .WriteElementString("numInputs", numInputs.ToString)
            .WriteElementString("LayerCount", network.LayerCount.ToString)
            .WriteElementString("numOutputs", numOutputs.ToString)

            .WriteStartElement("Layers")
            Dim l As Integer = 0
            For Each layer As Layer In network.Layers
                .WriteStartElement("Layer")

                .WriteAttributeString("Index", l.ToString)
                .WriteAttributeString("NeuronCount", layer.NeuronCount.ToString)
                .WriteAttributeString("Type", layer.LayerType.ToString)
                .WriteAttributeString("ActivationFunction", layer.ActivationFunction.ToString)

                .WriteEndElement() 'Layer

                l += 1
            Next

            .WriteEndElement() 'Layers
            .WriteEndElement() 'Network-Parameters

            ' Weights and biases
            .WriteStartElement("Weights-and-Biases")

            For layer = 0 To network.LayerCount - 1 'for each layer
                Dim currentLayer As Layer = network.Layers(layer)

                .WriteStartElement("Layer")
                .WriteAttributeString("Index", layer.ToString)

                For i = 0 To currentLayer.NeuronCount - 1

                    .WriteStartElement("Neuron")

                    .WriteAttributeString("Index", i.ToString)
                    .WriteAttributeString("Bias", currentLayer.Bias(i).ToString)

                    If layer = network.LayerCount - 1 Then 'then we are at the output layer - it has no weights saved, so skip to prevent NPE
                        'do nothing
                    Else
                        Dim nextLayer As Layer = network.Layers(layer + 1)
                        For j = 0 To nextLayer.NeuronCount - 1
                            .WriteStartElement("Connection")
                            .WriteAttributeString("Index", j.ToString)

                            .WriteString(currentLayer.Weights(i, j).ToString())

                            .WriteEndElement() 'Connection
                        Next
                    End If

                    .WriteEndElement() 'Neuron
                Next
                .WriteEndElement() 'layer
            Next

            .WriteEndElement() 'Weights-and-Biases

            .WriteEndElement() 'Network

            .Close()
        End With
    End Sub

    Private doc As XmlDocument = Nothing
    Public Function Load(ByRef network As BackpropagationNetwork, FilePath As String) As Boolean
        On Error GoTo ErrorCode

        If FilePath = Nothing Then
            Return False
        End If

        doc = New XmlDocument
        doc.Load(FilePath)

        Dim BasePath As String, numLayers As Integer, NeuronPath As String


        BasePath = "Network/Network-Parameters/"
        numLayers = CInt(xPathValue(BasePath & "LayerCount"))

        BasePath &= "Layers/Layer"

        'create layers
        For l = 0 To numLayers - 1
            Dim neuronCount As Integer = CInt(xPathValue((BasePath & "[@Index='") + l.ToString() + "']/@NeuronCount"))
            Dim activationFunction As ActivationFunction = [Enum].Parse(GetType(ActivationFunction), xPathValue((BasePath & "[@Index='") + l.ToString() + "']/@ActivationFunction"))
            Dim layerType As ILayer.LayerType_ = [Enum].Parse(GetType(ILayer.LayerType_), xPathValue((BasePath & "[@Index='") + l.ToString() + "']/@Type"))

            network.AddLayer(New Layer(neuronCount, activationFunction, layerType))
        Next

        'assign weights and biases
        'first assign random values and then re-assign new values (lazy). This avoids the NRE.
        Dim i As Integer = 0
        For Each layer As Layer In network.Layers
            If layer.LayerType = ILayer.LayerType_.Output Then
                Exit For
            End If
            layer.GenerateWeights(network, i)
            i += 1
        Next

        For Each layer As Layer In network.Layers
            layer.GenerateBias(network)
        Next

        'now assign new values
        For layer = 0 To network.LayerCount - 1 'for each layer
            BasePath = "Network/Weights-and-Biases/Layer[@Index='" & layer.ToString & "']/"

            Dim currentLayer As Layer = network.Layers(layer)

            For i = 0 To currentLayer.NeuronCount - 1
                NeuronPath = "Neuron[@Index='" + i.ToString + "']/@Bias"

                currentLayer.Bias(i) = CDbl(xPathValue(BasePath & NeuronPath))

                If layer = network.LayerCount - 1 Then 'then we are at the output layer - it has no weights saved, so skip to prevent error
                    'do nothing
                Else
                    Dim nextLayer As Layer = network.Layers(layer + 1)
                    For j = 0 To nextLayer.NeuronCount - 1
                        Dim connectionPath As String
                        connectionPath = "Neuron[@Index='" + i.ToString + "']/Connection[@Index='" + j.ToString + "']"
                        currentLayer.Weights(i, j) = CDbl(xPathValue(BasePath & connectionPath))
                    Next
                End If
            Next
        Next

        doc = Nothing

        Return True

ErrorCode:
        Return False

    End Function

    Private Function xPathValue(xPath As String) As String
        Dim node As XmlNode = doc.SelectSingleNode(xPath)

        If node Is Nothing Then
            Throw New ArgumentException("Cannot find specified node", xPath)
        End If

        Return node.InnerText
    End Function
End Module
