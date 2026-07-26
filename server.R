# server.R - kör bägge apparnas serverlogik i samma session.
# Inga output-id:n krockar mellan flikarna, så de kan dela input/output/session.

function(input, output, session) {
  foretag_server(input, output, session)
  handel_server(input, output, session)
}
