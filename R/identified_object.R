#' Identified object
#'
#' @description This class definition implements the same class from the OGC
#'   geoAPI Java package, extended with specific code for this package. This
#'   class is ancestor to many other, more useful classes. This class just
#'   provides basic identification properties. It is not very useful to
#'   instantiate this class directly; instead, use descendant classes.
#'
#' @docType class
IdentifiedObject <- R6::R6Class("IdentifiedObject",
  private = list(
    # MANDATORY The object name is set at initialization and is immutable
    .name       = NA_character_,

    # OPTIONAL Zero or more aliases for the object name
    .aliases    = list(),

    # OPTIONAL Zero or more registry identifiers for the object
    .identifiers = list(),

    # OPTIONAL Remarks for the object
    .remarks    = NA_character_
  ),
  public = list(
    #' @description Create a new object.
    #' @param name Character string. Name of the object.
    #' @return A new instance of the object, or an error if the object could not
    #'   be created.
    initialize = function(name) {
      if (is.character(name) && length(name) == 1L && nzchar(name))
        private$.name <- name
      else
        stop("Object name must be a character string.", call. = FALSE)
    },

    #' @description Retrieve the name of the object. This method is mandatory in
    #' the OGC standard.
    #' @return Character string with the name of the object.
    getName = function() {
      private$.name
    },

    #' @description Set aliases for the object. The aliases are added at the end
    #' of the defined aliases.
    #' @param alias Character vector with aliases for the object.
    #' @return Self, invisibly.
    setAlias = function(alias) {
      if (is.character(alias) && all(nzchar(alias)))
        private$.aliases <- alias
      else
        stop("Object aliases must be set using a character vector.", call. = FALSE)
      invisible(self)
    },

    #' @description Retrieve the list of aliases for the name of the object.
    #' This method is optional in the OGC standard.
    #' @return List of aliases for the name of the object.
    getAlias = function() {
      private$.aliases
    },

    #' @description Set identifiers for the object. The identifiers are added at the end
    #' of the defined identifiers.
    #' @param id Character vector with identifiers for the object.
    #' @return Self, invisibly.
    setIdentifiers = function(id) {
      if (is.character(alias) && all(nzchar(id)))
        private$.identifiers <- id
      else
        stop("Object identifiers must be set using a character vector.", call. = FALSE)
      invisible(self)
    },

    #' @description Retrieve the list of identifiers of the object. This method
    #'   is optional in the OGC standard.
    #' @return List of identifiers for the object.
    getIdentifiers = function() {
      private$.identifiers
    },

    #' @description Set remarks for the object.
    #' @param remarks Character string with remarks for the object.
    #' @return Self, invisibly.
    setRemarks = function(remarks) {
      if (is.character(remarks) && length(remarks) == 1L && nzchar(remarks))
        private$.remarks <- remarks
      else
        stop("Object remarks must be set using a character string.", call. = FALSE)
      invisible(self)
    },

    #' @description Retrieve the remarks of the object. This method is optional
    #'   in the OGC standard.
    #' @return Character string with remarks or `NA` if no remarks have been
    #'   set.
    getRemarks = function() {
      private$.remarks
    }
  ),
  active = list(
    #' @field name (read-only) The name of the object.
    name = function(value) {
      if (missing(value))
        private$.name
    }

  )
)
