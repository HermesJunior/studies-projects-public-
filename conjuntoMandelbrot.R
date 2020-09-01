# Hermes LUiz Bolinellli Junior, Eng MSc.
# Criação GIF Mandelbrot (Ref: Objetos Fractais: Benoit Madelbrot 2Ed, pag258-264)
# R: Utilizando pacote caTools (breve descrição): pacote externo que fornece várias funções utilitárias básicas: 
library(caTools)        

fCor = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
mAux = 1200                # var.auxiliar para definir tamanho

# criando o conjunto M de Mandelbrot, conforme referencia (pag258-264):
#
nCx = complex( real=rep(seq(-1.8,0.6, length.out=mAux), each=mAux ), #cria Numeros complexos
             imag=rep(seq(-1.2,1.2, length.out=mAux), mAux ) )
nCx = matrix(nCx,mAux,mAux)         # remodelar para uma matriz quadrada (mAuxxmAux) de números complexos
Z = 0                           # inicializa Z para zero
xPoints = array(0, c(mAux,mAux,50)) # inicializar matriz 3D de saída 
for (ic in 1:50) {              # Loop com 50 iterações
  Z = Z^2 + nCx                   # a equação de diferença central
  xPoints[,,ic] = exp(-abs(Z))  #  resultados da captura
}

# Grava a figura (gif) no diretoria corrente:
write.gif(xPoints, "Mandelbrot.gif", col=fCor, delay=100)
