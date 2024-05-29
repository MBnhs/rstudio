# Use uma imagem base do R
FROM r-base

# Instale pacotes necessários, se houver
RUN apt-get update && apt-get install -y \
    r-cran-ggplot2

# Copie seu script R para o contêiner
COPY r_script.R /

COPY participantes.csv /home

# Execute o script quando o contêiner for iniciado
CMD ["Rscript", "/r_script.R"]
