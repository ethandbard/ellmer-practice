# Quick sanity check that configured API keys work.
# Does not start Shiny. Costs a fraction of a cent per provider.

library(ellmer)

ok_any <- FALSE

if (nzchar(Sys.getenv("XAI_API_KEY"))) {
  message("Testing XAI_API_KEY with grok-4.5 ...")
  chat <- chat_openai_compatible(
    base_url = "https://api.x.ai/v1",
    name = "xAI",
    model = "grok-4.5",
    credentials = function() Sys.getenv("XAI_API_KEY")
  )
  reply <- chat$chat("Reply with exactly: grok-ok", echo = "none")
  cat(as.character(reply), "\n")
  ok_any <- TRUE
} else {
  message("XAI_API_KEY is empty. Add it with usethis::edit_r_environ(), then restart R.")
}

if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
  message("Testing ANTHROPIC_API_KEY with claude-haiku-4-5 ...")
  chat <- chat_anthropic(model = "claude-haiku-4-5")
  reply <- chat$chat("Reply with exactly: claude-ok", echo = "none")
  cat(as.character(reply), "\n")
  ok_any <- TRUE
} else {
  message("ANTHROPIC_API_KEY is empty. Add it with usethis::edit_r_environ(), then restart R.")
}

if (!ok_any) {
  stop("No API key is set. Run source(\"setup.R\") first.")
}

print(token_usage())
