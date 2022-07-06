puts "Bem-vindo ao Jogo da Advinhação"
puts "Qual o seu nome?"
nome = gets
puts ("\n\n\n")
#\n para cada linha em branco - refatoração
puts "Neste jogo você tem de advinhar qual o número escolhido entre 0 e 100.
A cada número escolhido vou lhe dar dicas para que você saiba se está perto de acertar."
puts ("\n\n\n")
puts "Vamos começar, " + nome
puts "Escolhendo um número..."
num_secreto = 34
puts "Pronto. Número escolhido, " + nome
puts ("\n\n")
puts "Você tem 3 tentativas. Vamos a primeira?"
#em breve, inserir a limitação de tentativas
puts "Chute um número, " + nome
chute = gets
puts "Será que você acertou? Você chutou " + chute
puts 34 == chute.to_i
# método to_i converte uma string para um inteiro




