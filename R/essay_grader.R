
#' Grading Essays or Generating Responses with ChatGPT
#'
#' This function grades essays or generates responses based on given prompts
#' and texts using a specified model.
#' To run this function, you need to have your own ChatGPT API key.
#'
#' @param text A vector of text strings to be graded or used as input for generating responses.
#' @param prompt A prompt or question that the text should respond to or be graded against.
#' @param model The specific model to be used for grading or generating responses (e.g., "gpt-3.5-turbo").
#'
#' @return A data frame with generated responses or graded results for each input text.
#'
#' @import progress
#' @export
#'
#' @examples
#' # Assign your own ChatGPT API key
#' my_API <- "your-api-key-here"
#'
#' # Define example texts and prompt
#' texts <- c("This is the first essay.", "Here is another essay.")
#' prompt <- "Evaluate the quality of this essay."
#'
#' # Specify the model to use
#' model <- "gpt-3.5-turbo"
#'
#' # Run the essay_grader function
#' graded_essays <- essay_grader(texts, prompt, model)
#'
#' # View the results
#' print(graded_essays)
#' @export

essay_grader <- function(text, prompt, model) {
  # Initialize a progress bar for tracking progress
  pb <- progress::progress_bar$new(
    format = "  [:bar] :percent :elapsed/:eta",
    total = length(text),
    clear = FALSE, width = 60
  )

  # Create a results data frame to store the outputs
  results <- as.data.frame(matrix(nrow = length(text), ncol = 1))

  # Loop through each text entry to process and grade or generate a response
  for (i in 1:length(text)) {
    # Combine the prompt and text, ensuring proper formatting
    prompt_text <- paste("\"", prompt, "\"", text[i])

    # Use the specified model to generate a response or grade the essay
    results[i, 1] <- ChatGPT(prompt_text, model)

    # Update the progress bar
    pb$tick()
  }

  # Return the generated results as a data frame
  return(results)
}

#' Interact with the ChatGPT API
#'
#' This function interacts with the ChatGPT API to generate responses based on a given prompt.
#' The function allows for a global fallback mechanism for the API key if not provided explicitly.
#'
#' @rdname essay_grader
#' @param prompt A string containing the prompt to be sent to the ChatGPT model.
#' @param model A string specifying the model to be used, with "gpt-3.5-turbo" as the default.
#' @param my_API A string containing the ChatGPT API key. If NULL, the function will attempt to use an API key stored in the global environment.
#' @param temp A numeric value between 0 and 1 that controls the randomness of the model's output, with 0 as the default for deterministic results.
#'
#' @return A string containing the generated response from the ChatGPT model.
#'
#' @import httr
#' @import stringr
#' @export
#'
#' @examples
#' # Example usage with explicit API key
#' response <- ChatGPT("What is the capital of France?", model = "gpt-3.5-turbo", my_API = "your-api-key-here")
#'
#' # Example usage with global API key
#' my_API <- "your-api-key-here"
#' response <- ChatGPT("What is the capital of France?", model = "gpt-3.5-turbo")
#'
#' # Example usage with different temperature
#' response <- ChatGPT("Generate a creative story", model = "gpt-3.5-turbo", temp = 0.7)
#'
#' @export

ChatGPT <- function(prompt, model = "gpt-3.5-turbo", my_API = NULL, temp = 0) {

  # Check if the API key is provided as an argument
  if (is.null(my_API)) {
    # If the API key is not provided, try to retrieve it from the global environment
    if (exists("my_API", envir = .GlobalEnv)) {
      my_API <- get("my_API", envir = .GlobalEnv)
    } else {
      # Stop the function if the API key is neither provided nor found globally
      stop("API key (my_API) is required.")
    }
  }

  # Make the API call, with error handling
  GPT_answer <- tryCatch(
    {
      # Send a POST request to the ChatGPT API
      POST(
        url = "https://api.openai.com/v1/chat/completions",  # API endpoint
        add_headers(Authorization = paste("Bearer", my_API)),  # Add the API key to the request headers
        content_type("application/json"),  # Specify the content type
        encode = "json",  # Encode the body as JSON
        body = list(
          model = model,  # Specify the model to use
          temperature = temp,  # Set the temperature for the response
          messages = list(
            list(
              role = "user",  # Specify the role of the message sender
              content = prompt  # The prompt to send to the model
            )
          )
        )
      )
    },
    error = function(e) {
      # Handle any errors that occur during the API request
      stop("API request failed: ", e$message)
    }
  )

  # Extract and validate the content of the response
  response_content <- content(GPT_answer)

  if (!is.null(response_content$choices) && length(response_content$choices) > 0) {
    # Return the cleaned response if it exists
    return(str_trim(response_content$choices[[1]]$message$content))
  } else {
    # Handle unexpected responses
    stop("Unexpected API response: ", response_content)
  }
}
