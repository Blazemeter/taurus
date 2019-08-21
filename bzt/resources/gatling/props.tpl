#########################
# Gatling Configuration #
#########################

gatling {
  core {
    outputDirectoryBaseName = "%(outputDirectoryBaseName)s"
    runDescription = "%(runDescription)s"
    encoding = "utf-8"
    directory {
      simulations = %(simulations)s
      resources = %(resources)s
      #binaries = ""                        # If set, name of the folder where compiles classes are located: Defaults to GATLING_HOME/target.
      results = %(results)s
    }
  }

  data {
    file {
      bufferSize = %(bufferSize)s            # FileDataWriter's internal data buffer size, in bytes
    }
  }
}