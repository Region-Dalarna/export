# server.R - kör bägge apparnas serverlogik i samma session.
# Inga output-id:n krockar mellan flikarna, så de kan dela input/output/session.

function(input, output, session) {
  # telemetry blir NULL när appen körs lokalt (fel värdnamn/inget lösenord
  # tillgängligt) - telemetri_server() skyddar tyvärr inte mot det själv, så
  # vi hoppar bara över anropet lokalt. På servrarna finns telemetry alltid.
  if (!is.null(telemetry)) {
    telemetri_server(telemetry, navigation_id = "huvudflik", forsta_flik = "Företagsstatistik")
  }

  foretag_server(input, output, session)
  handel_server(input, output, session)
}
