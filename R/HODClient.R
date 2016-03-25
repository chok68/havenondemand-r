# @class HODClient class to consume HPE Haven OnDemand API
#
# @version 1.0
# @author Topcoder

library(httr)

# Constructor
# @param apiKey Api Key of the user
# @param version Version of the api call should be made (v1, v2)
HODClient <- function(apikey = NULL, version = HODClientConstants$DEFAULT_VERSION)
{

  ## Get the environment for this
  ## instance of the function.
  thisEnv <- environment()

  apikey <- apikey

  version <- version

  ## Create the list used to represent an
  ## object for this class
  me <- list(

    ## Define the environment where this list is defined so
    ## that I can refer to it later.
    thisEnv = thisEnv,

    ## Define the accessors for the data fields.
    getEnv = function()
    {
      return(get("thisEnv",thisEnv))
    },

    # apikey getter
    getAPIKey = function()
    {
      return(get("apikey",thisEnv))
    },

    # apikey setter
    setAPIKey = function(value)
    {
      return(assign("apikey",value,thisEnv))
    },


    # version getter
    getVersion = function()
    {
      return(get("version",thisEnv))
    },

    # version getter
    setVersion = function(value)
    {
      return(assign("version",value,thisEnv))
    },

    #
    # Get URL to call the endpoint
    #
    # @param hodApp method to call
    # @param version version no. (v1, v2)
    # @mode SYNC/ASYNC
    getUrlString = function(hodApp, version, mode) {
      callMode = tolower(c(mode))
      return (paste(HODClientConstants$HOD_URL, HODClientConstants$HOD_API_URL, "/", callMode, hodApp, "/", version, sep=""))
    },

    # Get URL to get job status
    getJobStatusURLString = function()
    {
      return (paste(HODClientConstants$HOD_URL, "/", HODClientConstants$JOB_STATUS, sep = ""))
    },

    # Get URL to get job result
    getJobResultURLString = function()
    {
      return (paste(HODClientConstants$HOD_URL, "/", HODClientConstants$JOB_RESULT, sep = ""))
    },

    # calls POST Request
    #
    # @param hodApp endpoint to be called
    # @param params params to be passed
    # @param mode sync/async
    # @return json response (returned response varies depending on the endpoint called, check the API docs: https://dev.havenondemand.com/apis)
    postRequest = function(hodApp, params, mode)
    {
      apikey = get("apikey",thisEnv)
      version = get("version",thisEnv)

      url = me$getUrlString(hodApp = hodApp, version = version, mode = mode)

      return(me$invokeHODApi(url, apikey, params))
    },

    # Send request to HOD API server
    # Call appropiate endpoint  method
    # All the calls are passed through this method
    # @param path Path for the API
    # @param apikey API Key of the user
    # @param params data to be passed
    # @return json response (returned response varies depending on the endpoint called, check the API docs: https://dev.havenondemand.com/apis)
    invokeHODApi = function(path, apikey, params)
    {
      # add apikey to request body
      body = c(apikey = apikey, params)

      # post request
      response <- httr::POST(path, body = body)

      # error checking
      errorCode = httr::content(response, 'parsed')$error
      if (any(!is.null(errorCode), errorCode > 0)) stop (paste('Error detected. Reason: ', httr::content(response, 'parsed')$reason, 'Detail:', httr::content(response, 'parsed')$detail))

      # parse response (not done before error checking as parsing can fail)
      parsedResponse = httr::content(response, 'parsed')

      return(parsedResponse)
    },

    # Get status of the job submitted
    # @param jobId id of the job submitted
    # @return json response (returned response varies depending on the endpoint called, check the API docs: https://dev.havenondemand.com/apis)
    getJobStatus = function(jobID)
    {

      jobStatusUrl = me$getJobStatusURLString()

      url = paste(jobStatusUrl, jobID, sep = "")

      # add apikey to request body
      apikey = get("apikey",thisEnv)

      return(me$invokeHODApi(url, apikey, list()))
    },

    # Get result of the job submitted
    # @param jobId id of the job submitted
    # @return json response (returned response varies depending on the endpoint called, check the API docs: https://dev.havenondemand.com/apis)
    getJobResult = function(jobID)
    {
      jobResultUrl = me$getJobResultURLString()

      url = paste(jobResultUrl, jobID, sep = "")

      # add apikey to request body
      apikey = get("apikey",thisEnv)

      return(me$invokeHODApi(url, apikey, list()))
    }
  )

  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)

  ## Set the name for the class
  class(me) <- append(class(me),"HODClient")
  return(me)
}
