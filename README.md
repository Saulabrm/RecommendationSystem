# Recommendation System

Recommendation System for Text Mining Course by using the RGutenberg library

The objective of this work is to classify the books into one of the following categories: *Anger, Happiness, Fear, Sadness, Shame and Guilt* These categories were chosen by using the [Karla Mclaren](http://karlamclaren.com/emotional-vocabulary-page/) list of emotions, to further built a dictionary to use as a bag of words .

The Books are chosen randomly from the `RGutenberg` library which contains a vast amount of books ready to be downloaded into a dataset in `R`. 

Pipeline:
- Build a dictionary.
- Import the dataset of books
- Preprocess the text (Remove punctuation, lowercase, stemming)
- Use cosine distance to find a relation with each emotion
- Assign the emotion to the book that best describes it.
- User: Choose a book
- Recommendation based on the emotion assigned to the chosen book.

In this model, the recommendation is basically filtering the book by the same emotion as the author's choice, and choosing randomly three books which are consider similar by emotion. If there is not other 3 books in the database, then it will randomly recommend some other book.

Future work: In this model we are not considering other users, but recommendation could also include the popularity factor of each book and possibly implement the similarity factor of each user.

# The Code

The project was built in `R` and also an application in `Shiny`, to run them, the dictionary has to be downloaded into the same folder, and to install all the packages needed.

