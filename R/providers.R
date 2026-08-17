# Provider + model catalog for the chat client (Claude and xAI / Grok).

provider_catalog <- list(
  xai = list(
    id = "xai",
    label = "xAI (Grok)",
    env = "XAI_API_KEY",
    default = "grok-4.5",
    models = c(
      "Grok 4.5" = "grok-4.5",
      "Grok 4.6 (latest)" = "grok-4.6",
      "Grok 4.3 (cheaper)" = "grok-4.3"
    )
  ),
  anthropic = list(
    id = "anthropic",
    label = "Anthropic (Claude)",
    env = "ANTHROPIC_API_KEY",
    default = "claude-haiku-4-5",
    models = c(
      "Haiku 4.5 (fast)" = "claude-haiku-4-5",
      "Sonnet 4.6 (stronger)" = "claude-sonnet-4-6",
      "Sonnet 5" = "claude-sonnet-5"
    )
  )
)

has_provider_key <- function(provider) {
  spec <- provider_catalog[[provider]]
  !is.null(spec) && nzchar(Sys.getenv(spec$env))
}

available_providers <- function() {
  names(provider_catalog)[vapply(names(provider_catalog), has_provider_key, logical(1))]
}

startup_provider_id <- function() {
  avail <- available_providers()
  if ("anthropic" %in% avail) {
    return("anthropic")
  }
  if ("xai" %in% avail) {
    return("xai")
  }
  NULL
}

provider_choices <- function() {
  stats::setNames(
    vapply(provider_catalog, `[[`, "", "id"),
    vapply(provider_catalog, `[[`, "", "label")
  )
}

require_any_provider_key <- function() {
  if (!is.null(startup_provider_id())) {
    return(invisible(TRUE))
  }
  stop(
    "No API key found. Set at least one of these in your user .Renviron, then restart R:\n",
    "  XAI_API_KEY=xai-your-key\n",
    "  ANTHROPIC_API_KEY=sk-ant-your-key\n",
    "Run usethis::edit_r_environ() to open that file.\n",
    "xAI key: https://console.x.ai\n",
    "Claude key: https://platform.claude.com/settings/keys",
    call. = FALSE
  )
}

make_chat_client <- function(provider, model, system_prompt = NULL) {
  spec <- provider_catalog[[provider]]
  if (is.null(spec)) {
    stop("Unknown provider: ", provider, call. = FALSE)
  }
  if (!has_provider_key(provider)) {
    stop(
      spec$env, " is not set.\n",
      "Add this line to your user .Renviron (usethis::edit_r_environ()), save, restart R:\n",
      "  ", spec$env, "=your-key-here",
      call. = FALSE
    )
  }
  if (identical(provider, "anthropic")) {
    return(ellmer::chat_anthropic(model = model, system_prompt = system_prompt))
  }
  if (identical(provider, "xai")) {
    return(ellmer::chat_openai_compatible(
      base_url = "https://api.x.ai/v1",
      name = "xAI",
      model = model,
      system_prompt = system_prompt,
      credentials = function() Sys.getenv("XAI_API_KEY")
    ))
  }
  stop("Unknown provider: ", provider, call. = FALSE)
}

# querychat keeps one Chat object in a closure. Swap its provider in place
# so the next user message uses the new backend without rebuilding the module.
#
# This reaches into ellmer 0.4.2 internals (`private$provider`). Guarded by
# packageVersion(); on failure the caller should rebuild the Chat object.
attach_chat_provider <- function(live_chat, new_chat) {
  if (!ellmer_supported("0.4.2")) {
    warning("ellmer < 0.4.2: in-place provider swap is unsupported.", call. = FALSE)
    return(FALSE)
  }
  ok <- tryCatch({
    live_chat$.__enclos_env__$private$provider <- new_chat$get_provider()
    live_chat$set_turns(list())
    TRUE
  }, error = function(e) {
    warning("attach_chat_provider failed: ", conditionMessage(e), call. = FALSE)
    FALSE
  })
  ok
}

llm_status_label <- function(provider, model) {
  spec <- provider_catalog[[provider]]
  paste0(spec$label %or% provider, " · ", model)
}

key_setup_note <- function(provider) {
  spec <- provider_catalog[[provider]]
  paste0(
    spec$env, " is missing. Add it with usethis::edit_r_environ(), then restart R."
  )
}
