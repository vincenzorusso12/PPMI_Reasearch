# PPMI Research
The main goal is to create a system in order to classify in a quick way healthy people from  people suffering from Parkinson's disease.
We have developed a process based on [Latent Semantic Analysis](https://en.wikipedia.org/wiki/Latent_semantic_analysis), which finds hidden meanings in a text. For instance ignoring synonims.
We have extended the process using a neural network technique [text2vec](http://text2vec.org/) that gives a numerical representation on the documents and finds the most similar.
Both technologies produce a similiarity matrix ([cosine](https://en.wikipedia.org/wiki/Cosine_similarity) distance). Then we use clustering algorithms to classify the patients.
We've find out that the most accurate techniques, benchmarked using [F-measure](https://en.wikipedia.org/wiki/F1_score), were text2vec with K-means and LSA with Fuzzy.
Proposals for further development could be a better training of the neural network text2vec.

## For further information check the PowerPoint in this repo
## [Google Drive](https://drive.google.com/drive/folders/1sRIe9kS6KRqHCH_b1nMZNds7uumLMPts?usp=sharing)
