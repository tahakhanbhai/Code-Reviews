# Assignment 3
# Michelle Starkell

# Set dictionary of words to be used in the hangman game
dictionary <- c("carrot", "spinach", "turnip", "beet", "broccoli", "pepper", "onion", "cucumber")

# Save dictionary of words in project directory
file_path <- "dictionary.txt"
writeLines(dictionary, file_path)

# Read the wordlist from the program
wordlist <- readLines(file_path)

# Choose one random word from the wordlist
word <- sample(wordlist, 1)

# Inform the user on the length of the word and the number of incorrect guesses allowed. 
print(paste("The word is", (nchar(word)), "letters long. You are allowed 5 incorrect guesses"))

# Create a variable for the number of guesses remaining
guesses_left <- 5

# Initialize a vector for the guessed letters to store guessed letters during the game
guessed_letters <- c()

# Initialize a vector to store guessed letters that are incorrect
incorrect_letters <- c()

# Initialize the variable game_over to flag whether the game should continue or end
game_over <- FALSE

# Initialize an empty string which will be used to visualize the word being guessed
updated_word <- ""

# Initialize an empty string for the guess variable
guess <- ""

# Print a welcome message for the user before the loop begins
cat("Welcome to Hangman: Vegetable Edition!\n")
# Create a while loop to prompt user to enter a letter until the game is over
while (guesses_left > 0 && 
       # loop only runs if the user has guesses left
       game_over == FALSE && 
       # loop only runs if the game is not over
       (!(guess %in% word)) &&
       # loop only runs if the user has not guessed all of the correct letters
       (!(updated_word %in% word))) { 
  # loop only runs if the user has not guessed the word
  # create a prompt to get user to answer a letter and convert the user input to lowercase. 
  # Alternatively, allow the user to guess the entire word
  answer <- tolower(readline(prompt = "Please enter a letter or type 'guess' to guess the word: "))
  # check if user input is a letter
  if (grepl("^[a-zA-Z]$", answer)) {
    # check if letter has already been guessed and inform the user
    if (answer %in% guessed_letters) {
      print("That letter has already been guessed. Please try again.")
    } else {
      # If letter has not been guessed, add it to the guessed letters vector
      guessed_letters <- c(guessed_letters, answer)
      # Check to see if the guessed letter is in the word 
      if (answer %in% strsplit(word, "")[[1]]) {
        # If user input is in the word, inform the user
        print(paste("Good guess!", answer, "is in the word"))
        # Initialize an empty string variable to keep track of how many letters in the word the player has guessed
        updated_word <- ""
        # Loop goes over each character in the word and updates it according to user's guess
        for (char in strsplit(word, "")[[1]]) {
          # Checks if the character is in the guessed_letters vector
          if (char %in% guessed_letters) {
            # If the character is present in the guessed_letters vector, it will be added to the updated_word variable
            updated_word <- paste(updated_word, char, sep = "")
          } else {
            # If the character is not present in the guessed_letters vector, an underscore will be added to the updated_word variable
            updated_word <- paste(updated_word, "_", sep = "")
          }
        }
        # Print the updated word to visually inform user of their progress
        print(updated_word)
        # If user input is not in the word...
      } else {
        # Subtract one guess from user's remaining guesses
        guesses_left <- guesses_left - 1
        # Update the incorrect letters vector to include the new guess
        incorrect_letters <- c(incorrect_letters, answer)
        # Inform the user of how many guesses they have remaining
        print(paste(answer, "is not in the word! Remaining attempts:", guesses_left))
        # Inform the user of the their incorrect guesses
        print(paste("Incorrect guesses:", paste(incorrect_letters, collapse = ", ")))
        # If the user has no guesses left, the game ends
        if (guesses_left == 0) {
          game_over <- TRUE
        }
        # If the game ends, inform the user of the correct word
        if (game_over == TRUE) {
          print(paste("Game over. The word was",word, ". Better luck next time!"))
        }
      }
      # If the word has been guessed, inform the user that they have won the game
      if (updated_word %in% word) {
        print(paste("You won! The word was", word))
      }
    }
  } else {
    # If user types guess, let them guess the word
    if (answer == "guess") {
      # prompt the user to enter a guess
      guess <- tolower(readline(prompt = "Guess the word: ")) 
      # Check if guessed word has already been guessed
      if (guess %in% incorrect_letters) {
        # If guessed word has been guessed, inform the user
        print("That word has already been guessed. Please try again.")
      } else {
        # If the word has been guessed correctly, inform the user and end the game
        if (guess %in% word) {
          print(paste("Congratulations! You won! The word is",word,"!"))
        } else {
          # If the word has not been guessed correctly, substract one guess from remaining guesses
          guesses_left <- guesses_left - 1
          # Update the incorrect letters vector to include the guessed word
          incorrect_letters <- c(incorrect_letters, guess)
          # Inform the user of how many guesses they have remaining
          print(paste(answer, "is not in the word! Remaining attempts:", guesses_left))
          # Inform the user of the their incorrect guesses
          print(paste("Incorrect guesses:", paste(incorrect_letters, collapse = ", ")))
          # If the user has no guesses left, the game ends
        } 
        if (guesses_left == 0) {
          game_over <- TRUE
        }
        # If the game ends, inform the user
        if (game_over == TRUE) {
          print(paste("Game over. The word was",word, ". Better luck next time!"))
        }
      }
    } else {
      # If the user input is not a single letter, restart the loop and ask the user for input again
      print("That is not a letter. Try again!")
    }
  }
}

