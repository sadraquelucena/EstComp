# logo criada com https://www.design.com/

library(hexSticker)

sticker(
  subplot = "/home/sadraque/Downloads/websitetest/img/logo.png",   # sua imagem retangular
  s_x = 1, s_y = 1, s_width = 1.2,  # ajustes da imagem
  package = "",        # nome do pacote/curso
  p_size = 16,                  # tamanho do texto
  p_color = "white",           # cor do texto
  h_fill = "#BED5BE",           # cor do fundo
  h_color = "#2A6420",          # cor da borda
  white_around_sticker = TRUE,   # remove tudo fora do hexágono
  asp = 1,                       # mantém proporção
  filename = "img/ESTAT0090_logo_hex.png"     # nome do arquivo final
)

library(magick)
p <- image_read("img/ESTAT0090_logo_hex.png")
pp <- p %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = 4, point = "+1+1") %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = 4, point = "+517+1") %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = 4, point = "+1+599") %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = 4, point = "+517+599")
image_write(image = pp, path = "img/ESTAT0090_logo_hex.png")
file.show("img/ESTAT0090_logo_hex.png")
