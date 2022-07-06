# Deep Learning

- [x] A presença de Machine Learning nas diferentes áreas
- [x] Dificuldade de criar sistemas com representações e extração de features
- [x] _Deep learning_ como um tipo de _representation learning_ (hierarquia, abstração, etc.)
- [x] Backpropagation
- [x] CNNs e RNNs

_Machine learning_ está presente em muitos dos sistemas com os quais nós interagimos no nosso dia-a-dia. Os sistemas de busca na internet, o reconhecimento de amigos em fotos nas redes sociais e a interface de voz no celular são exemplos disso. Boa parte desses sistemas fazem uso de um tipo de técnicas que vem sendo chamado de _deep learning_.

Por muito tempo, as técnicas de Machine learning dependiam da identificação de _features_ relevantes nos dados, de modo que para construir um sistema que atuasse em um dado domínio era necessário ter um grande conhecimento a respeito do domínio para determinar quais eram as características relevantes e como elas deveriam ser identificadas nos dados brutos.

Por exemplo, para construir um sistema que analisasse automaticamente os _reviews_ de um filme por meio de processamento de linguagem natural, era necessário primeiramente determinar quais eram as etapas de pre-processamento relevantes para a tarefa, como por exemplo a extração dos radicais das palavras ou a remoção de palavras muito frequentes como "e", "com" e "que". A seguir, seria necessário identificar quais palavras ou frases deveriam funcionar como "características" de um review positivo e quais seriam características de um review negativo.

Uma vez definidos esses diferentes tipos de "características", os dados poderiam então ser *representados* de uma maneira que explicitasse a presença das diferentes características e um classificador _baysiano_ poderia ser usado para determinar se um dado review deveria ser considerado positivo ou negativo.
Uma ideia semelhante pode ser aplicada a diferentes áreas como classificação de imagens, identificação de objetos, reconhecimento de voz, etc.

A construção de sistemas assim requer um trabalho muito cuidadoso e um grande domínio da área em questão para que se possa extrair um conjunto relevante de características e, ao mesmo tempo, desprezar variações insignificantes.[1]

A ideia por trás do Deep Learning é tornar a extração e identificação das características relevantes do dado bruto algo que o sistema aprenda a partir de um grande número de exemplos sem que seja necessário programar explicitamente as _features_ a serem identificadas. Desta forma, _deep learning_ pode ser visto como uma forma de _representation learning_. A substituição da necessidade de especialização e da criação cuidadosa de um sistema que extraia as características dos dados por um sistema que aprenda a fazer isso automaticamente a partir de um número muito grande de exemplos se faz ainda mais interessante em uma época em que se tantos dados são criados e publicados na internet diariamente.

O grande desafio na área de _machine learning_ se torna conseguir dados suficientes para treinar o sistema e construir uma arquitetura que seja capaz de aprender.

A partir dos dados _raw_, cada camada de uma rede de deep learning é capaz de aprender representações cada vez mais abstratas dos dados de modo a ressaltar características que sejam relevantes enquanto despreza variações que não alteram o resultado. Por exemplo, para reconhecer a foto de um cachorro, as camadas mais inferiores da rede aprenderão a identificar bordas; camadas mais superiores irão aprender a identificar, a partir da representação gerada pelas camadas anteriores - que já terão identificado as bordas da imagem -, formas e padrões relevantes, enquanto desprezam pequenas variações de tamanho ou localização; alguma camada mais acima será capaz de, a partir dos padrões e formas identificadas anteriormente, identificar os olhos ou o focinho do cachorro. Estas diferentes representações vão se tornando cada vez mais abstratas, de modo que a camada de output seja capaz de identificar se há ou não um cachorro na imagem[1].

Um sistema de deep learning é uma rede neural artificial[2], ou seja, possui um número de unidades (neurônios) dispostos em camadas. Cada neurônio em uma camada está ligado aos neurônios da camada seguinte e esta ligação tem um valor chamado _peso_ que determina a intensidade com a qual a ativação de um neurônio deve influenciar a ligação do neurônio seguinte. Um neurônio recebe as várias ativações dos neurônios que estão ligados a ele multiplicadas pelos respectivos pesos. Essa soma é então transformada por uma _função de ativação_ (em geral a ReLU, mas outras funções possíveis são a tangente hiperbólica e a função logística) e o valor resultante será a ativação que este neurônio passará aos neurônios aos quais estiver ligado.

A maioria das aplicações de _deep learning_ utilizam aprendizado supervisionado. Nesse tipo de aprendizado, o sistema é treinado com um grande número de exemplos já classificados. Os pesos da rede são inicializados aleatoriamente. Cada exemplo do conjunto de treinamento é composto por uma entrada semelhante à que o sistema deverá classificar posteriormente e o alvo, ou resposta certa. O sistema processa a entrada por meio das ativações dos neurônios até que uma resposta seja gerada na camada de output. O sistema vai sendo exposto a cada exemplo do conjunto de treino e, à medida que os dados vão sendo apresentados, é calculado um erro a partir da diferença entre o resultado dado pelo sistema e o resultado esperado. O processo de aprendizado pode ser definido como um problema de otimização em que a função de erro deve ser minimizada.

Para minimizar a função objetivo, utiliza-se o Gradiente Descendente Estocástico[3]. A partir do erro calculado e do derivada parcial de cada peso da camada de output, é possível calcular em que direção cada peso deve ser atualizado para caminhar na direção do mínimo global da função de erro. Este mesmo processo é repetido para a camada anterior e assim por diante até atualizar todos os pesos da rede. A este processo de atualização sucessiva dos pesos até que se minimize a função de erro é dado o nome de _backpropagation_[2, ??], uma vez que o erro é propagado a partir da camada de output para as camadas anteriores e é basicamente uma aplicação prática da regra da cadeia para as derivadas.

Dois tipos importantes de redes de deep learning são as redes convolucionais[4, 5], que tem sido amplamente usadas principalmente para o processamento de imagem e visão computacional, e as redes recorrentes, que são usadas especialmente para dados sequenciais, como áudio e linguagem natural.

As redes convolucionais são compostas de diferentes tipos de camadas: _convolução_, _pooling_ e _fully connected_. Nas camadas de convolução um filtro é aplicado à imagem de modo a destacar algum aspecto. O filtro funciona como um kernel que irá percorrer a imagem. O tamanho e a quantidade filtros de uma camada de convolução é um hiper parâmetro que deve ser determinado na arquitetura do modelo. A forma do filtro, ou seja, a matriz que o define, é aprendida ao longo do processo de treino. As camadas de pooling diminuem a amostragem por meio de uma função que reduz a dimensionalidade da matriz 2D a ser passada para a próxima camada. Um tipo muito popular de pooling é o _max pooling_ em que é escolhido o valor máximo de uma região da imagem. Outra possilidade seria o uso da média dos pixels de uma região.

```
| 10 12  7  8 13  9 | 
|  8  8 10 11  7 10 |
|  9  8  9 14 15 13 |           | 12 15 |
| 10 11  9 13 12 12 |     ->    | 11 13 |
|  9  9  8  9 10 10 |
|  8  9  8  9  9  7 |  

exemplo de max pooling

max(10, 12, 7, 8, 8, 10, 9, 8, 9) = 12
max(8, 13, 9, 11, 7, 10, 14, 15, 13) = 15
...
```

As camadas de pooling permitem que certas variações de escala ou translação sejam desprezadas. O fato de o pixel máximo estar mais à direita ou mais à esquerda não altera o resultado do pooling. Desta forma, a presença de uma característica relevante em diferentes posições da imagem pode ser capturada independentemente da sua localização exata.

As redes recorrentes[6] recebem este nome porque a camada escondida tem uma ligação para si mesma. Esta rede é usada para o processamento de dados sequenciais e esta ligação da saída de um item com a entrada do próximo a ser processado faz com que seja possível à rede guardar informações relevantes de itens passados para o processamento do item atual. A ligação recorrente funciona como uma espécie de memória de curta duração.

Entretanto, foi provado teórica e praticamente que as RNNs não são capazes de guardar as informações por muitas iterações[7]. Como solução para este problema tem-se usado Long Short-Term Memories[8]. Esta é uma arquitetura que contém uma memória esplícita para o processamento de sequências de itens. A LSTM contém uma memória que a rede aprende a usar para guardar informações relevantes dos itens anteriores e para apagar informações que devem ser desprezadas. A LSTM funciona por meio de ligações a partir da camada escondida com peso 1, ou seja, que permitem que os dados sejam copiados para a memória, mas contendo "gates" multiplicativos que podem ser ajustados para remover informações que não sejam úteis.


## Referências
- [1] LeCun, Bengio, Hinton (2015)
- [2] Rumelhart (1986)
- [3] Bottou, L. & Bousquet, O. The tradeoffs of large scale learning. In Proc. Advances in Neural Information Processing Systems 20 161–168 (2007)
- [4] LeCun,Y.etal.Handwrittendigitrecognitionwithaback-propagationnetwork. In Proc. Advances in Neural Information Processing Systems 396–404
- [5] LeCun, Y., Bottou, L., Bengio, Y. & Haffner, P. Gradient-based learning applied to document recognition. Proc. IEEE 86, 2278–2324 (1998).
- [6] Socher, R., Lin, C. C-Y., Manning, C. & Ng, A. Y. Parsing natural scenes andnatural language with recursive neural networks. In Proc. InternationalConference on Machine Learning 129–136 (2011)
- [7] Bengio, Y., Simard, P. & Frasconi, P. Learning long-term dependencies withgradient descent is difficult. IEEE Trans. Neural Networks 5, 157–166 (1994).
- [8] Hochreiter, S. & Schmidhuber, J. Long short-term memory. Neural Comput. 9,1735–1780 (1997)
