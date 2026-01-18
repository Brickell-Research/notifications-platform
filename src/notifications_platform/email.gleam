import mork

/// Render markdown to HTML
pub fn markdown_to_html(markdown: String) -> String {
  markdown
  |> mork.parse
  |> mork.to_html
}

/// Wrap HTML content in an email-safe template with Miami-style CSS
pub fn wrap_html_email(content_html: String, unsubscribe_link: String) -> String {
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>Brickell Research</title>
</head>
<body style=\"margin: 0; padding: 0; background-color: #000000; font-family: Monaco, 'Courier New', monospace;\">
  <table role=\"presentation\" width=\"100%\" cellspacing=\"0\" cellpadding=\"0\" style=\"border-collapse: collapse; background-color: #000000;\">
    <tr>
      <td align=\"center\" style=\"padding: 30px 20px;\">
        <table role=\"presentation\" width=\"600\" cellspacing=\"0\" cellpadding=\"0\" style=\"border-collapse: collapse; background-color: rgba(0,0,0,0.9); border: 2px solid #00CED1;\">
          <!-- Header -->
          <tr>
            <td style=\"padding: 30px 40px; border-bottom: 1px solid #00CED1; text-align: center;\">
              <table role=\"presentation\" width=\"100%\" cellspacing=\"0\" cellpadding=\"0\">
                <tr>
                  <td align=\"center\">
                    <img src=\"https://brickellresearch.org/brickell_research_logo.png\" alt=\"Brickell Research\" width=\"48\" height=\"48\" style=\"display: block; margin: 0 auto 15px auto;\">
                    <h1 style=\"margin: 0; font-family: Georgia, serif; color: #FF1493; font-size: 24px; font-weight: bold;\">Brickell Research</h1>
                  </td>
                </tr>
              </table>
            </td>
          </tr>
          <!-- Content -->
          <tr>
            <td style=\"padding: 40px; color: #E0FFFF; font-size: 15px; line-height: 1.8;\">
              <style>
                a { color: #00CED1 !important; }
                h1, h2, h3, h4, h5, h6 { color: #FF1493 !important; font-family: Georgia, serif; }
                p { color: #E0FFFF; margin: 1em 0; }
                ul, ol { color: #E0FFFF; }
                code { background: rgba(0, 206, 209, 0.2); padding: 2px 6px; color: #00CED1; }
                pre { background: rgba(0, 206, 209, 0.1); padding: 15px; border: 1px solid #00CED1; overflow-x: auto; }
                pre code { background: none; padding: 0; }
                blockquote { border-left: 3px solid #FF1493; padding-left: 15px; margin-left: 0; color: #5f9ea0; }
                hr { border: none; border-top: 1px solid #00CED1; margin: 20px 0; }
              </style>
              " <> content_html <> "
            </td>
          </tr>
          <!-- Footer -->
          <tr>
            <td style=\"padding: 25px 40px; background-color: #001a1a; border-top: 1px solid #00CED1; text-align: center; font-size: 12px; color: #5f9ea0;\">
              <p style=\"margin: 0 0 15px 0; color: #5f9ea0;\">Brickell Research</p>
              <!--[if mso]>
              <v:roundrect xmlns:v=\"urn:schemas-microsoft-com:vml\" xmlns:w=\"urn:schemas-microsoft-com:office:word\" href=\"" <> unsubscribe_link <> "\" style=\"height:40px;v-text-anchor:middle;width:150px;\" arcsize=\"10%\" strokecolor=\"#00CED1\" fillcolor=\"#001a1a\">
                <w:anchorlock/>
                <center style=\"color:#00CED1;font-family:Monaco,monospace;font-size:14px;\">Unsubscribe</center>
              </v:roundrect>
              <![endif]-->
              <!--[if !mso]><!-->
              <table role=\"presentation\" cellspacing=\"0\" cellpadding=\"0\" border=\"0\" align=\"center\">
                <tr>
                  <td style=\"border-radius: 4px; border: 1px solid #00CED1; background-color: #001a1a;\">
                    <a href=\"" <> unsubscribe_link <> "\" target=\"_blank\" style=\"display: block; padding: 12px 24px; font-family: Monaco, monospace; font-size: 14px; color: #00CED1; text-decoration: none; text-align: center;\">Unsubscribe</a>
                  </td>
                </tr>
              </table>
              <!--<![endif]-->
              <p style=\"margin: 15px 0 0 0; font-size: 10px; color: #5f9ea0;\">
                Or copy this link: " <> unsubscribe_link <> "
              </p>
            </td>
          </tr>
        </table>
      </td>
    </tr>
  </table>
</body>
</html>"
}

/// Create plain text version from markdown (strip formatting)
pub fn markdown_to_plain_text(
  markdown: String,
  unsubscribe_link: String,
) -> String {
  markdown <> "\n\n---\nTo unsubscribe: " <> unsubscribe_link
}

/// Build a complete email with both HTML and plain text versions
pub type EmailContent {
  EmailContent(plain_text: String, html: String)
}

/// Render a template body (markdown) into email content
pub fn render_template(body: String, unsubscribe_link: String) -> EmailContent {
  let html_content = markdown_to_html(body)
  let full_html = wrap_html_email(html_content, unsubscribe_link)
  let plain_text = markdown_to_plain_text(body, unsubscribe_link)

  EmailContent(plain_text: plain_text, html: full_html)
}

/// Render template for preview (HTML only, no unsubscribe link needed)
pub fn render_preview(body: String) -> String {
  let html_content = markdown_to_html(body)
  wrap_html_email(html_content, "#")
}
