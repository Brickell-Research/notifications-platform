import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import lustre/attribute
import lustre/element
import lustre/element/html
import notifications_platform/sql
import youid/uuid

pub type Tab {
  Home
  Subscribers
  Templates
  Send
  History
  Analytics
}

pub fn login_page(error: Option(String), csrf_token: String) -> String {
  html.html([], [
    html.head([], [
      html.title([], "Admin Login"),
      html.style([], styles()),
    ]),
    html.body([], [
      html.div([attribute.class("container")], [
        html.div([attribute.class("header")], [
          logo(),
          html.div([], [
            html.h1([], [element.text("Brickell Research")]),
            html.p([attribute.class("subtitle")], [element.text("Admin Login")]),
          ]),
        ]),
        case error {
          Some(msg) -> html.p([attribute.class("error")], [element.text(msg)])
          None -> element.none()
        },
        html.form([attribute.method("POST"), attribute.action("/admin/login")], [
          html.input([
            attribute.type_("hidden"),
            attribute.name("csrf_token"),
            attribute.value(csrf_token),
          ]),
          html.div([attribute.class("field")], [
            html.label([attribute.for("email")], [element.text("Email")]),
            html.input([
              attribute.type_("email"),
              attribute.name("email"),
              attribute.id("email"),
              attribute.required(True),
            ]),
          ]),
          html.div([attribute.class("field")], [
            html.label([attribute.for("password")], [element.text("Password")]),
            html.input([
              attribute.type_("password"),
              attribute.name("password"),
              attribute.id("password"),
              attribute.required(True),
            ]),
          ]),
          html.button([attribute.type_("submit")], [element.text("Login")]),
        ]),
      ]),
    ]),
  ])
  |> element.to_document_string()
}

pub fn admin_dashboard(active_tab: Tab) -> String {
  html.html([], [
    html.head([], [
      html.title([], "Admin Dashboard"),
      html.style([], styles()),
    ]),
    html.body([], [
      html.div([attribute.class("container wide")], [
        logout_button(),
        html.div([attribute.class("header")], [
          logo(),
          html.div([], [
            html.h1([], [element.text("Brickell Research")]),
            html.p([attribute.class("subtitle")], [
              element.text("Notifications Admin"),
            ]),
          ]),
        ]),
        tabs(active_tab),
        html.div([attribute.class("content")], [
          case active_tab {
            Home -> home_content()
            Subscribers -> element.text("Loading...")
            Templates -> element.text("Loading...")
            Send -> element.text("Loading...")
            History -> element.text("Loading...")
            Analytics -> element.text("Loading...")
          },
        ]),
        footer(),
      ]),
    ]),
  ])
  |> element.to_document_string()
}

pub type Message {
  Success(String)
  Error(String)
}

pub fn subscribers_page(
  subscribers: List(sql.GetAllSubscribersRow),
  message: Option(Message),
) -> String {
  html.html([], [
    html.head([], [
      html.title([], "Admin - Subscribers"),
      html.style([], styles()),
    ]),
    html.body([], [
      html.div([attribute.class("container wide")], [
        logout_button(),
        html.div([attribute.class("header")], [
          logo(),
          html.div([], [
            html.h1([], [element.text("Brickell Research")]),
            html.p([attribute.class("subtitle")], [
              element.text("Notifications Admin"),
            ]),
          ]),
        ]),
        tabs(Subscribers),
        html.div([attribute.class("content")], [
          case message {
            Some(Success(msg)) ->
              html.p([attribute.class("success-msg")], [element.text(msg)])
            Some(Error(msg)) ->
              html.p([attribute.class("error-msg")], [element.text(msg)])
            None -> element.none()
          },
          subscribers_content(subscribers),
        ]),
        footer(),
      ]),
      html.script([], subscribers_scripts()),
    ]),
  ])
  |> element.to_document_string()
}

fn tabs(active: Tab) -> element.Element(a) {
  html.nav([attribute.class("tabs")], [
    html.a(
      [
        attribute.href("/admin"),
        attribute.class(case active {
          Home -> "tab active"
          _ -> "tab"
        }),
      ],
      [element.text("Home")],
    ),
    html.a(
      [
        attribute.href("/admin/subscribers"),
        attribute.class(case active {
          Subscribers -> "tab active"
          _ -> "tab"
        }),
      ],
      [element.text("Subscribers")],
    ),
    html.a(
      [
        attribute.href("/admin/templates"),
        attribute.class(case active {
          Templates -> "tab active"
          _ -> "tab"
        }),
      ],
      [element.text("Templates")],
    ),
    html.a(
      [
        attribute.href("/admin/send"),
        attribute.class(case active {
          Send -> "tab active"
          _ -> "tab"
        }),
      ],
      [element.text("Send")],
    ),
    html.a(
      [
        attribute.href("/admin/history"),
        attribute.class(case active {
          History -> "tab active"
          _ -> "tab"
        }),
      ],
      [element.text("History")],
    ),
    html.a(
      [
        attribute.href("/admin/analytics"),
        attribute.class(case active {
          Analytics -> "tab active"
          _ -> "tab"
        }),
      ],
      [element.text("Analytics")],
    ),
  ])
}

fn home_content() -> element.Element(a) {
  html.div([], [
    html.p([], [element.text("Welcome to the notifications admin panel.")]),
    html.p([], [
      element.text("Use the tabs above to manage subscribers and templates."),
    ]),
  ])
}

fn logo() -> element.Element(a) {
  html.div([attribute.class("logo")], [
    html.img([
      attribute.src("https://brickellresearch.org/brickell_research_logo.png"),
      attribute.attribute("width", "48"),
      attribute.attribute("height", "48"),
      attribute.alt("Brickell Research"),
    ]),
  ])
}

fn logout_button() -> element.Element(a) {
  html.form(
    [
      attribute.method("POST"),
      attribute.action("/admin/logout"),
      attribute.class("logout-form"),
    ],
    [html.button([attribute.type_("submit")], [element.text("Logout")])],
  )
}

fn footer() -> element.Element(a) {
  html.div([attribute.class("footer")], [
    element.text("systems thinking, without the thinking | "),
    html.a(
      [
        attribute.href("https://brickellresearch.org"),
        attribute.target("_blank"),
      ],
      [
        element.text("brickellresearch.org"),
      ],
    ),
  ])
}

fn subscribers_scripts() -> String {
  "(function() {
    var table = document.getElementById('subscribers-table');
    var search = document.getElementById('subscribers-search');
    var countEl = document.getElementById('subscribers-count');
    if (!table) return;

    var tbody = table.querySelector('tbody');
    var headers = table.querySelectorAll('th.sortable');
    var currentSort = { col: null, dir: null };

    headers.forEach(function(th) {
      th.addEventListener('click', function() {
        var col = parseInt(th.dataset.col);
        var type = th.dataset.type;
        var dir = 'asc';
        if (currentSort.col === col && currentSort.dir === 'asc') {
          dir = 'desc';
        }
        headers.forEach(function(h) { h.classList.remove('asc', 'desc'); });
        th.classList.add(dir);
        currentSort = { col: col, dir: dir };

        var rows = Array.from(tbody.querySelectorAll('tr'));
        rows.sort(function(a, b) {
          var aVal = a.cells[col].textContent.trim().toLowerCase();
          var bVal = b.cells[col].textContent.trim().toLowerCase();
          var cmp = 0;
          if (aVal < bVal) cmp = -1;
          else if (aVal > bVal) cmp = 1;
          return dir === 'asc' ? cmp : -cmp;
        });
        rows.forEach(function(row) { tbody.appendChild(row); });
      });
    });

    if (search) {
      search.addEventListener('input', function() {
        var query = search.value.toLowerCase();
        var rows = tbody.querySelectorAll('tr');
        var visibleCount = 0;
        rows.forEach(function(row) {
          var email = row.cells[0].textContent.toLowerCase();
          var matches = email.includes(query);
          row.classList.toggle('hidden-row', !matches);
          if (matches) visibleCount++;
        });
        if (countEl) {
          countEl.textContent = 'Showing: ' + visibleCount + ' of ' + rows.length;
        }
      });
    }
  })();"
}

fn subscribers_content(
  subscribers: List(sql.GetAllSubscribersRow),
) -> element.Element(a) {
  html.div([], [
    // Add subscriber form
    html.form(
      [
        attribute.method("POST"),
        attribute.action("/admin/subscribers/add"),
        attribute.class("add-form"),
      ],
      [
        html.input([
          attribute.type_("email"),
          attribute.name("email"),
          attribute.placeholder("email@example.com"),
          attribute.required(True),
        ]),
        html.button([attribute.type_("submit")], [element.text("Add")]),
      ],
    ),
    html.div([attribute.class("table-controls")], [
      html.input([
        attribute.type_("text"),
        attribute.id("subscribers-search"),
        attribute.placeholder("Search by email..."),
        attribute.class("search-input"),
      ]),
      html.p([attribute.class("count"), attribute.id("subscribers-count")], [
        element.text(
          "Total subscribers: " <> int.to_string(list.length(subscribers)),
        ),
      ]),
    ]),
    case subscribers {
      [] ->
        html.p([attribute.class("empty")], [element.text("No subscribers yet.")])
      _ ->
        html.table(
          [
            attribute.class("subscribers-table"),
            attribute.id("subscribers-table"),
          ],
          [
            html.thead([], [
              html.tr([], [
                html.th(
                  [
                    attribute.class("sortable"),
                    attribute.attribute("data-col", "0"),
                    attribute.attribute("data-type", "string"),
                  ],
                  [
                    element.text("Email"),
                    html.span([attribute.class("sort-indicator")], []),
                  ],
                ),
                html.th(
                  [
                    attribute.class("sortable"),
                    attribute.attribute("data-col", "1"),
                    attribute.attribute("data-type", "string"),
                  ],
                  [
                    element.text("Status"),
                    html.span([attribute.class("sort-indicator")], []),
                  ],
                ),
                html.th(
                  [
                    attribute.class("sortable"),
                    attribute.attribute("data-col", "2"),
                    attribute.attribute("data-type", "string"),
                  ],
                  [
                    element.text("ID"),
                    html.span([attribute.class("sort-indicator")], []),
                  ],
                ),
                html.th([], []),
              ]),
            ]),
            html.tbody(
              [],
              list.map(subscribers, fn(sub) {
                html.tr([], [
                  html.td([], [element.text(sub.email)]),
                  html.td(
                    [
                      attribute.class(case sub.confirmed {
                        True -> "status confirmed"
                        False -> "status pending"
                      }),
                    ],
                    [
                      element.text(case sub.confirmed {
                        True -> "Confirmed"
                        False -> "Pending"
                      }),
                    ],
                  ),
                  html.td([attribute.class("uuid")], [
                    element.text(uuid.to_string(sub.id)),
                  ]),
                  html.td([attribute.class("actions")], [
                    html.form(
                      [
                        attribute.method("POST"),
                        attribute.action("/admin/subscribers/delete"),
                        attribute.attribute(
                          "onsubmit",
                          "return confirm('Are you sure you want to delete this subscriber?')",
                        ),
                      ],
                      [
                        html.input([
                          attribute.type_("hidden"),
                          attribute.name("email"),
                          attribute.value(sub.email),
                        ]),
                        html.button(
                          [
                            attribute.type_("submit"),
                            attribute.class("delete-btn"),
                          ],
                          [element.text("Delete")],
                        ),
                      ],
                    ),
                  ]),
                ])
              }),
            ),
          ],
        )
    },
  ])
}

pub fn templates_page(
  templates: List(sql.ListTemplatesRow),
  message: Option(Message),
) -> String {
  html.html([], [
    html.head([], [
      html.title([], "Admin - Templates"),
      html.style([], styles()),
    ]),
    html.body([], [
      html.div([attribute.class("container wide")], [
        logout_button(),
        html.div([attribute.class("header")], [
          logo(),
          html.div([], [
            html.h1([], [element.text("Brickell Research")]),
            html.p([attribute.class("subtitle")], [
              element.text("Notifications Admin"),
            ]),
          ]),
        ]),
        tabs(Templates),
        html.div([attribute.class("content")], [
          case message {
            Some(Success(msg)) ->
              html.p([attribute.class("success-msg")], [element.text(msg)])
            Some(Error(msg)) ->
              html.p([attribute.class("error-msg")], [element.text(msg)])
            None -> element.none()
          },
          templates_content(templates),
        ]),
        footer(),
      ]),
    ]),
  ])
  |> element.to_document_string()
}

fn templates_content(
  templates: List(sql.ListTemplatesRow),
) -> element.Element(a) {
  html.div([], [
    html.a(
      [attribute.href("/admin/templates/new"), attribute.class("btn-add")],
      [
        element.text("+ New Template"),
      ],
    ),
    html.p([attribute.class("count")], [
      element.text("Total templates: " <> int.to_string(list.length(templates))),
    ]),
    case templates {
      [] ->
        html.p([attribute.class("empty")], [element.text("No templates yet.")])
      _ ->
        html.table([attribute.class("templates-table")], [
          html.thead([], [
            html.tr([], [
              html.th([], [element.text("Name")]),
              html.th([], [element.text("Subject")]),
              html.th([], []),
            ]),
          ]),
          html.tbody(
            [],
            list.map(templates, fn(template) {
              html.tr([], [
                html.td([], [element.text(template.name)]),
                html.td([attribute.class("subject")], [
                  element.text(template.subject),
                ]),
                html.td([attribute.class("actions")], [
                  html.a(
                    [
                      attribute.href(
                        "/admin/templates/"
                        <> uuid.to_string(template.id)
                        <> "/preview",
                      ),
                      attribute.attribute("target", "_blank"),
                      attribute.class("preview-btn-sm"),
                    ],
                    [element.text("Preview")],
                  ),
                  html.a(
                    [
                      attribute.href(
                        "/admin/templates/" <> uuid.to_string(template.id),
                      ),
                      attribute.class("edit-btn"),
                    ],
                    [element.text("Edit")],
                  ),
                  html.form(
                    [
                      attribute.method("POST"),
                      attribute.action("/admin/templates/delete"),
                      attribute.class("inline-form"),
                      attribute.attribute(
                        "onsubmit",
                        "return confirm('Are you sure you want to delete this template?')",
                      ),
                    ],
                    [
                      html.input([
                        attribute.type_("hidden"),
                        attribute.name("id"),
                        attribute.value(uuid.to_string(template.id)),
                      ]),
                      html.button(
                        [
                          attribute.type_("submit"),
                          attribute.class("delete-btn"),
                        ],
                        [element.text("Delete")],
                      ),
                    ],
                  ),
                ]),
              ])
            }),
          ),
        ])
    },
  ])
}

pub fn template_edit_page(
  template: Option(sql.GetTemplateByIdRow),
  message: Option(Message),
) -> String {
  let is_new = option.is_none(template)
  let title = case is_new {
    True -> "New Template"
    False -> "Edit Template"
  }
  let form_action = case template {
    Some(t) -> "/admin/templates/" <> uuid.to_string(t.id)
    None -> "/admin/templates/new"
  }
  let name_val = case template {
    Some(t) -> t.name
    None -> ""
  }
  let subject_val = case template {
    Some(t) -> t.subject
    None -> ""
  }
  let body_val = case template {
    Some(t) -> t.body
    None -> ""
  }

  html.html([], [
    html.head([], [
      html.title([], "Admin - " <> title),
      html.style([], styles()),
    ]),
    html.body([], [
      html.div([attribute.class("container wide")], [
        logout_button(),
        html.div([attribute.class("header")], [
          logo(),
          html.div([], [
            html.h1([], [element.text("Brickell Research")]),
            html.p([attribute.class("subtitle")], [
              element.text("Notifications Admin"),
            ]),
          ]),
        ]),
        html.div([attribute.class("breadcrumb")], [
          html.a([attribute.href("/admin/templates")], [
            element.text("← Back to Templates"),
          ]),
        ]),
        html.h2([attribute.class("page-title")], [element.text(title)]),
        html.div([attribute.class("content")], [
          case message {
            Some(Success(msg)) ->
              html.p([attribute.class("success-msg")], [element.text(msg)])
            Some(Error(msg)) ->
              html.p([attribute.class("error-msg")], [element.text(msg)])
            None -> element.none()
          },
          html.form(
            [
              attribute.method("POST"),
              attribute.action(form_action),
              attribute.class("template-form"),
            ],
            [
              html.div([attribute.class("field")], [
                html.label([attribute.for("name")], [element.text("Name")]),
                html.input([
                  attribute.type_("text"),
                  attribute.name("name"),
                  attribute.id("name"),
                  attribute.value(name_val),
                  attribute.required(True),
                  attribute.placeholder("e.g. welcome-email"),
                ]),
              ]),
              html.div([attribute.class("field")], [
                html.label([attribute.for("subject")], [element.text("Subject")]),
                html.input([
                  attribute.type_("text"),
                  attribute.name("subject"),
                  attribute.id("subject"),
                  attribute.value(subject_val),
                  attribute.required(True),
                  attribute.placeholder("e.g. Welcome to Brickell Research"),
                ]),
              ]),
              html.div([attribute.class("field")], [
                html.label([attribute.for("body")], [
                  element.text("Body (Markdown)"),
                ]),
                element.element(
                  "textarea",
                  [
                    attribute.name("body"),
                    attribute.id("body"),
                    attribute.attribute("rows", "12"),
                    attribute.required(True),
                    attribute.placeholder(
                      "Write your email content in markdown...",
                    ),
                  ],
                  [element.text(body_val)],
                ),
              ]),
              html.div([attribute.class("form-actions")], [
                html.button([attribute.type_("submit")], [
                  element.text(case is_new {
                    True -> "Create Template"
                    False -> "Save Changes"
                  }),
                ]),
                case template {
                  Some(t) ->
                    html.a(
                      [
                        attribute.href(
                          "/admin/templates/"
                          <> uuid.to_string(t.id)
                          <> "/preview",
                        ),
                        attribute.attribute("target", "_blank"),
                        attribute.class("preview-btn"),
                      ],
                      [element.text("Preview")],
                    )
                  None -> element.none()
                },
              ]),
            ],
          ),
        ]),
        footer(),
      ]),
    ]),
  ])
  |> element.to_document_string()
}

pub type SendResult {
  SendResult(sent: Int, failed: Int, errors: List(String))
}

pub fn send_page(
  templates: List(sql.ListTemplatesRow),
  subscriber_count: Int,
  message: Option(Message),
  result: Option(SendResult),
) -> String {
  html.html([], [
    html.head([], [
      html.title([], "Admin - Send"),
      html.style([], styles()),
      element.element("script", [], [element.text(send_page_script())]),
    ]),
    html.body([], [
      html.div([attribute.class("container wide")], [
        logout_button(),
        html.div([attribute.class("header")], [
          logo(),
          html.div([], [
            html.h1([], [element.text("Brickell Research")]),
            html.p([attribute.class("subtitle")], [
              element.text("Notifications Admin"),
            ]),
          ]),
        ]),
        tabs(Send),
        html.div([attribute.class("content")], [
          case message {
            Some(Success(msg)) ->
              html.p([attribute.class("success-msg")], [element.text(msg)])
            Some(Error(msg)) ->
              html.p([attribute.class("error-msg")], [element.text(msg)])
            None -> element.none()
          },
          case result {
            Some(r) -> send_result_display(r)
            None -> element.none()
          },
          send_form(templates, subscriber_count),
        ]),
        footer(),
      ]),
    ]),
  ])
  |> element.to_document_string()
}

fn send_page_script() -> String {
  ""
}

fn send_result_display(result: SendResult) -> element.Element(a) {
  html.div([attribute.class("send-result")], [
    html.p([attribute.class("result-summary")], [
      element.text(
        "Sent: "
        <> int.to_string(result.sent)
        <> " / Failed: "
        <> int.to_string(result.failed),
      ),
    ]),
    case result.errors {
      [] -> element.none()
      errors ->
        html.div([attribute.class("result-errors")], [
          html.p([], [element.text("Errors:")]),
          html.ul(
            [],
            list.map(errors, fn(err) { html.li([], [element.text(err)]) }),
          ),
        ])
    },
  ])
}

fn send_form(
  templates: List(sql.ListTemplatesRow),
  subscriber_count: Int,
) -> element.Element(a) {
  html.div([], [
    html.p([attribute.class("count")], [
      element.text("Subscribers: " <> int.to_string(subscriber_count)),
    ]),
    case templates {
      [] ->
        html.div([attribute.class("empty")], [
          html.p([], [element.text("No templates yet. ")]),
          html.a([attribute.href("/admin/templates/new")], [
            element.text("Create one first."),
          ]),
        ])
      _ ->
        html.form(
          [
            attribute.method("POST"),
            attribute.action("/admin/send"),
            attribute.class("send-form"),
          ],
          [
            html.div([attribute.class("field")], [
              html.label([attribute.for("template_id")], [
                element.text("Template"),
              ]),
              html.select(
                [
                  attribute.name("template_id"),
                  attribute.id("template_id"),
                  attribute.required(True),
                ],
                [
                  html.option([attribute.value("")], "Select a template..."),
                  ..list.map(templates, fn(t) {
                    html.option(
                      [attribute.value(uuid.to_string(t.id))],
                      t.name <> " - " <> t.subject,
                    )
                  })
                ],
              ),
            ]),
            html.div([attribute.class("field")], [
              html.label([], [element.text("Audience")]),
              html.div([attribute.class("radio-group")], [
                html.label([attribute.class("radio-label")], [
                  html.input([
                    attribute.type_("radio"),
                    attribute.name("audience"),
                    attribute.value("all"),
                    attribute.checked(True),
                  ]),
                  element.text(
                    " All subscribers ("
                    <> int.to_string(subscriber_count)
                    <> ")",
                  ),
                ]),
                html.div([attribute.class("radio-label single-email-row")], [
                  html.label([attribute.class("radio-label-inline")], [
                    html.input([
                      attribute.type_("radio"),
                      attribute.name("audience"),
                      attribute.value("single"),
                    ]),
                    element.text(" Single email: "),
                  ]),
                  html.input([
                    attribute.type_("email"),
                    attribute.name("single_email"),
                    attribute.id("single_email"),
                    attribute.placeholder("recipient@example.com"),
                    attribute.class("single-email-input"),
                  ]),
                ]),
              ]),
            ]),
            html.div([attribute.class("form-actions")], [
              html.button(
                [attribute.type_("submit"), attribute.class("send-btn")],
                [
                  element.text("Send Emails"),
                ],
              ),
            ]),
          ],
        )
    },
  ])
}

// --- History page ------------------------------------------------------------

pub fn history_page(history: List(sql.GetSendHistoryRow)) -> String {
  html.html([], [
    html.head([], [
      html.title([], "Admin - Send History"),
      html.style([], styles()),
    ]),
    html.body([], [
      html.div([attribute.class("container wide")], [
        logout_button(),
        html.div([attribute.class("header")], [
          logo(),
          html.div([], [
            html.h1([], [element.text("Brickell Research")]),
            html.p([attribute.class("subtitle")], [
              element.text("Notifications Admin"),
            ]),
          ]),
        ]),
        tabs(History),
        html.div([attribute.class("content")], [
          html.div([attribute.class("history-controls")], [
            html.input([
              attribute.type_("text"),
              attribute.id("history-search"),
              attribute.placeholder("Search by template or subject..."),
              attribute.class("search-input"),
            ]),
            html.p([attribute.class("count"), attribute.id("history-count")], [
              element.text(
                "Recent sends: " <> int.to_string(list.length(history)),
              ),
            ]),
          ]),
          history_table(history),
        ]),
        footer(),
      ]),
      html.script([], history_scripts()),
    ]),
  ])
  |> element.to_document_string()
}

fn history_table(history: List(sql.GetSendHistoryRow)) -> element.Element(a) {
  case history {
    [] ->
      html.p([attribute.class("empty"), attribute.id("empty-message")], [
        element.text("No sends yet."),
      ])
    _ ->
      html.table(
        [attribute.class("history-table"), attribute.id("history-table")],
        [
          html.thead([], [
            html.tr([], [
              html.th(
                [
                  attribute.class("sortable"),
                  attribute.attribute("data-col", "0"),
                  attribute.attribute("data-type", "date"),
                ],
                [
                  element.text("Date"),
                  html.span([attribute.class("sort-indicator")], []),
                ],
              ),
              html.th(
                [
                  attribute.class("sortable"),
                  attribute.attribute("data-col", "1"),
                  attribute.attribute("data-type", "string"),
                ],
                [
                  element.text("Type"),
                  html.span([attribute.class("sort-indicator")], []),
                ],
              ),
              html.th(
                [
                  attribute.class("sortable"),
                  attribute.attribute("data-col", "2"),
                  attribute.attribute("data-type", "string"),
                ],
                [
                  element.text("Template"),
                  html.span([attribute.class("sort-indicator")], []),
                ],
              ),
              html.th(
                [
                  attribute.class("sortable"),
                  attribute.attribute("data-col", "3"),
                  attribute.attribute("data-type", "string"),
                ],
                [
                  element.text("Subject"),
                  html.span([attribute.class("sort-indicator")], []),
                ],
              ),
              html.th(
                [
                  attribute.class("sortable"),
                  attribute.attribute("data-col", "4"),
                  attribute.attribute("data-type", "number"),
                ],
                [
                  element.text("Sent"),
                  html.span([attribute.class("sort-indicator")], []),
                ],
              ),
              html.th(
                [
                  attribute.class("sortable"),
                  attribute.attribute("data-col", "5"),
                  attribute.attribute("data-type", "number"),
                ],
                [
                  element.text("Failed"),
                  html.span([attribute.class("sort-indicator")], []),
                ],
              ),
            ]),
          ]),
          html.tbody(
            [],
            list.map(history, fn(row) {
              let send_type = case row.total_recipients {
                1 -> "One-off"
                _ -> "Blast"
              }
              html.tr([], [
                html.td([attribute.class("date")], [
                  element.text(row.created_at),
                ]),
                html.td(
                  [
                    attribute.class(case send_type {
                      "Blast" -> "send-type blast"
                      _ -> "send-type one-off"
                    }),
                  ],
                  [element.text(send_type)],
                ),
                html.td([], [element.text(row.template_name)]),
                html.td(
                  [
                    attribute.class("subject"),
                    attribute.title(row.template_subject),
                  ],
                  [element.text(row.template_subject)],
                ),
                html.td([attribute.class("sent-count")], [
                  element.text(int.to_string(row.sent_count)),
                ]),
                html.td(
                  [
                    attribute.class(case row.failed_count {
                      0 -> "failed-count"
                      _ -> "failed-count has-failures"
                    }),
                  ],
                  [element.text(int.to_string(row.failed_count))],
                ),
              ])
            }),
          ),
        ],
      )
  }
}

// --- Analytics page ----------------------------------------------------------

pub type TimeWindow {
  Days7
  Days30
  Days90
}

pub fn analytics_page(
  data: List(sql.GetEmailsByDayRow),
  current_window: TimeWindow,
) -> String {
  html.html([], [
    html.head([], [
      html.title([], "Admin - Analytics"),
      html.style([], styles()),
    ]),
    html.body([], [
      html.div([attribute.class("container wide")], [
        logout_button(),
        html.div([attribute.class("header")], [
          logo(),
          html.div([], [
            html.h1([], [element.text("Brickell Research")]),
            html.p([attribute.class("subtitle")], [
              element.text("Notifications Admin"),
            ]),
          ]),
        ]),
        tabs(Analytics),
        html.div([attribute.class("content")], [
          html.h2([attribute.class("page-title")], [element.text("Emails Sent")]),
          analytics_time_selector(current_window),
          analytics_chart(data, current_window),
          analytics_summary(data),
        ]),
        footer(),
      ]),
    ]),
  ])
  |> element.to_document_string()
}

fn analytics_time_selector(current: TimeWindow) -> element.Element(a) {
  html.div([attribute.class("analytics-time-selector")], [
    html.a(
      [
        attribute.href("/admin/analytics?window=7"),
        attribute.class(case current {
          Days7 -> "time-btn active"
          _ -> "time-btn"
        }),
      ],
      [element.text("7 Days")],
    ),
    html.a(
      [
        attribute.href("/admin/analytics?window=30"),
        attribute.class(case current {
          Days30 -> "time-btn active"
          _ -> "time-btn"
        }),
      ],
      [element.text("30 Days")],
    ),
    html.a(
      [
        attribute.href("/admin/analytics?window=90"),
        attribute.class(case current {
          Days90 -> "time-btn active"
          _ -> "time-btn"
        }),
      ],
      [element.text("90 Days")],
    ),
  ])
}

fn analytics_chart(
  data: List(sql.GetEmailsByDayRow),
  window: TimeWindow,
) -> element.Element(a) {
  let max_value =
    list.fold(data, 1, fn(acc, row) {
      case row.emails_sent > acc {
        True -> row.emails_sent
        False -> acc
      }
    })

  let bar_width = case window {
    Days7 -> "calc((100% - 6 * 8px) / 7)"
    Days30 -> "calc((100% - 29 * 4px) / 30)"
    Days90 -> "calc((100% - 89 * 2px) / 90)"
  }

  let bar_gap = case window {
    Days7 -> "8px"
    Days30 -> "4px"
    Days90 -> "2px"
  }

  case data {
    [] ->
      html.div([attribute.class("analytics-empty")], [
        element.text("No email data for this period."),
      ])
    _ ->
      html.div([attribute.class("analytics-chart-container")], [
        html.div([attribute.class("analytics-y-axis")], [
          html.span([attribute.class("y-label")], [
            element.text(int.to_string(max_value)),
          ]),
          html.span([attribute.class("y-label")], [
            element.text(int.to_string(max_value / 2)),
          ]),
          html.span([attribute.class("y-label")], [element.text("0")]),
        ]),
        html.div(
          [
            attribute.class("analytics-chart"),
            attribute.attribute("style", "gap: " <> bar_gap),
          ],
          list.map(data, fn(row) {
            let height_pct = case max_value {
              0 -> 0
              _ -> row.emails_sent * 100 / max_value
            }
            let height_str = int.to_string(height_pct) <> "%"
            html.div(
              [
                attribute.class("chart-bar-wrapper"),
                attribute.attribute("style", "width: " <> bar_width),
              ],
              [
                html.div(
                  [
                    attribute.class("chart-bar"),
                    attribute.attribute("style", "height: " <> height_str),
                    attribute.title(
                      row.day
                      <> ": "
                      <> int.to_string(row.emails_sent)
                      <> " emails",
                    ),
                  ],
                  [],
                ),
                case window {
                  Days7 ->
                    html.span([attribute.class("chart-label")], [
                      element.text(format_day_short(row.day)),
                    ])
                  _ -> element.none()
                },
              ],
            )
          }),
        ),
      ])
  }
}

fn format_day_short(day: String) -> String {
  // day is in format "YYYY-MM-DD", extract MM-DD
  case day {
    _ ->
      case list.length(day |> to_graphemes) >= 10 {
        True -> slice_string(day, 5, 5)
        False -> day
      }
  }
}

fn to_graphemes(s: String) -> List(String) {
  do_to_graphemes(s, [])
}

fn do_to_graphemes(s: String, acc: List(String)) -> List(String) {
  case s {
    "" -> list.reverse(acc)
    _ -> {
      let first = slice_string(s, 0, 1)
      let rest = slice_string(s, 1, 1000)
      do_to_graphemes(rest, [first, ..acc])
    }
  }
}

@external(erlang, "string", "slice")
fn slice_string(s: String, start: Int, length: Int) -> String

fn analytics_summary(data: List(sql.GetEmailsByDayRow)) -> element.Element(a) {
  let total = list.fold(data, 0, fn(acc, row) { acc + row.emails_sent })
  let avg = case list.length(data) {
    0 -> 0
    n -> total / n
  }

  html.div([attribute.class("analytics-summary")], [
    html.div([attribute.class("summary-stat")], [
      html.span([attribute.class("stat-value")], [
        element.text(int.to_string(total)),
      ]),
      html.span([attribute.class("stat-label")], [element.text("Total Sent")]),
    ]),
    html.div([attribute.class("summary-stat")], [
      html.span([attribute.class("stat-value")], [
        element.text(int.to_string(avg)),
      ]),
      html.span([attribute.class("stat-label")], [element.text("Daily Avg")]),
    ]),
  ])
}

fn styles() -> String {
  "
  * { box-sizing: border-box; }
  body {
    font-family: Monaco, 'Courier New', monospace;
    background: #000000;
    color: #E0FFFF;
    line-height: 1.8;
    margin: 0;
    padding: 2rem;
    min-height: 100vh;
  }
  .container {
    position: relative;
    max-width: 420px;
    margin: 40px auto;
    padding: 2rem;
    border: 2px solid #00CED1;
    background: rgba(255,255,255,0.03);
  }
  .wide { max-width: 750px; }

  /* Header with logo */
  .header {
    display: flex !important;
    flex-direction: row !important;
    align-items: center;
    gap: 1rem;
    margin-bottom: 1.5rem;
    padding-bottom: 1rem;
    border-bottom: 1px solid #00CED1;
  }
  .logo {
    flex-shrink: 0;
    display: inline-block;
  }
  .logo img {
    display: block;
    width: 48px;
    height: 48px;
  }
  h1 {
    font-family: Georgia, serif;
    font-weight: bold;
    font-size: 1.4rem;
    color: #FF1493;
    margin: 0;
    padding: 0;
    border: none;
  }
  .field {
    margin-bottom: 1.25rem;
  }
  label {
    display: block;
    margin-bottom: 0.35rem;
    font-size: 0.8rem;
    color: #E0FFFF;
    text-transform: uppercase;
    font-weight: bold;
  }
  input {
    width: 100%;
    padding: 0.75rem;
    border: 1px solid #00CED1;
    background: #000000;
    color: #E0FFFF;
    font-family: inherit;
    font-size: 0.9rem;
  }
  input:focus {
    outline: none;
    border-color: #FF1493;
  }
  input::placeholder {
    color: #5f9ea0;
  }
  button {
    padding: 0.75rem 1.5rem;
    background: #FF1493;
    color: #ffffff;
    border: none;
    font-family: inherit;
    font-size: 0.8rem;
    font-weight: bold;
    text-transform: uppercase;
    cursor: pointer;
  }
  button:hover {
    background: #ff69b4;
  }
  .error {
    color: #ff6b6b;
    font-size: 0.8rem;
    margin-bottom: 1rem;
    padding: 0.75rem;
    border: 1px solid #ff6b6b;
    background: rgba(255, 107, 107, 0.1);
  }
  .success-msg {
    color: #00CED1;
    font-size: 0.8rem;
    margin-bottom: 1rem;
    padding: 0.75rem;
    border: 1px solid #00CED1;
    background: rgba(0, 206, 209, 0.1);
  }
  .error-msg {
    color: #ffc107;
    font-size: 0.8rem;
    margin-bottom: 1rem;
    padding: 0.75rem;
    border: 1px solid #ffc107;
    background: rgba(255, 193, 7, 0.1);
  }
  p {
    margin: 1rem 0;
  }
  .subtitle {
    color: #00CED1;
    font-size: 0.8rem;
    margin: 0;
    text-transform: uppercase;
  }

  /* Tabs */
  .tabs {
    display: flex;
    gap: 0;
    border-bottom: 1px solid #00CED1;
    margin-bottom: 1.5rem;
  }
  .tab {
    padding: 0.6rem 1rem;
    text-decoration: none;
    color: #E0FFFF;
    border-bottom: 2px solid transparent;
    margin-bottom: -1px;
    text-transform: uppercase;
    font-size: 0.75rem;
    font-weight: bold;
  }
  .tab:hover {
    color: #FF1493;
    border-color: transparent;
  }
  .tab.active {
    color: #FF1493;
    border-bottom-color: #FF1493;
  }

  /* Content */
  .content {
    margin-bottom: 2rem;
  }
  .count {
    color: #00CED1;
    font-size: 0.8rem;
    text-transform: uppercase;
  }
  .empty {
    color: #5f9ea0;
    font-style: italic;
  }
  .logout-form {
    position: absolute;
    top: 1rem;
    right: 1rem;
  }
  .logout-form button {
    padding: 0.4rem 0.75rem;
    font-size: 0.7rem;
    background: transparent;
    border: 1px solid #ff6b6b;
    color: #ff6b6b;
  }
  .logout-form button:hover {
    background: #ff6b6b;
    color: #000;
  }

  /* Footer */
  .footer {
    margin-top: 2rem;
    padding-top: 1rem;
    border-top: 1px solid #00CED1;
    text-align: center;
    font-size: 0.75rem;
    color: #5f9ea0;
  }
  .footer a {
    color: #00CED1;
  }

  /* Table */
  .subscribers-table {
    width: 100%;
    border-collapse: collapse;
    margin-top: 1rem;
    border: 1px solid #00CED1;
  }
  .subscribers-table th,
  .subscribers-table td {
    padding: 0.75rem;
    text-align: left;
    border-bottom: 1px solid rgba(0, 206, 209, 0.3);
  }
  .subscribers-table th {
    background: rgba(0, 206, 209, 0.2);
    color: #FF1493;
    font-size: 0.7rem;
    text-transform: uppercase;
    font-weight: bold;
  }
  .subscribers-table tr:hover {
    background: rgba(0, 206, 209, 0.1);
  }
  .uuid {
    font-family: Monaco, monospace;
    font-size: 0.65rem;
    color: #5f9ea0;
  }

  /* Add form */
  .add-form {
    display: flex;
    gap: 0.5rem;
    margin-bottom: 1.5rem;
  }
  .add-form input {
    flex: 1;
    width: auto;
  }
  .add-form button {
    flex-shrink: 0;
  }

  /* Actions */
  .actions {
    text-align: right;
  }
  .delete-btn {
    padding: 0.4rem 0.75rem;
    font-size: 0.7rem;
    background: transparent;
    border: 1px solid #ff6b6b;
    color: #ff6b6b;
  }
  .delete-btn:hover {
    background: #ff6b6b;
    color: #000000;
  }

  a {
    color: #00CED1;
    text-decoration: none;
  }
  a:hover {
    color: #FF1493;
  }

  /* Templates table */
  .templates-table {
    width: 100%;
    border-collapse: collapse;
    margin-top: 1rem;
    border: 1px solid #00CED1;
  }
  .templates-table th,
  .templates-table td {
    padding: 0.75rem;
    text-align: left;
    border-bottom: 1px solid rgba(0, 206, 209, 0.3);
  }
  .templates-table th {
    background: rgba(0, 206, 209, 0.2);
    color: #FF1493;
    font-size: 0.7rem;
    text-transform: uppercase;
    font-weight: bold;
  }
  .templates-table tr:hover {
    background: rgba(0, 206, 209, 0.1);
  }
  .subject {
    color: #5f9ea0;
    font-size: 0.85rem;
  }

  /* Add template button */
  .btn-add {
    display: inline-block;
    padding: 0.75rem 1.5rem;
    background: #FF1493;
    color: #ffffff;
    font-size: 0.8rem;
    font-weight: bold;
    text-transform: uppercase;
    margin-bottom: 1rem;
  }
  .btn-add:hover {
    background: #ff69b4;
    color: #ffffff;
  }

  /* Edit button */
  .edit-btn {
    display: inline-block;
    padding: 0.4rem 0.75rem;
    font-size: 0.7rem;
    background: transparent;
    border: 1px solid #00CED1;
    color: #00CED1;
    margin-right: 0.5rem;
  }
  .edit-btn:hover {
    background: #00CED1;
    color: #000000;
  }

  /* Small preview button for tables */
  .preview-btn-sm {
    display: inline-block;
    padding: 0.4rem 0.75rem;
    font-size: 0.7rem;
    background: transparent;
    border: 1px solid #FF1493;
    color: #FF1493;
    margin-right: 0.5rem;
  }
  .preview-btn-sm:hover {
    background: #FF1493;
    color: #000000;
  }

  /* Inline form for delete in table */
  .inline-form {
    display: inline-block;
  }

  /* Breadcrumb */
  .breadcrumb {
    margin-bottom: 1rem;
    font-size: 0.8rem;
  }

  /* Page title */
  .page-title {
    color: #FF1493;
    font-size: 1.1rem;
    margin-bottom: 1.5rem;
    font-weight: bold;
  }

  /* Template form */
  .template-form textarea {
    width: 100%;
    padding: 0.75rem;
    border: 1px solid #00CED1;
    background: #000000;
    color: #E0FFFF;
    font-family: Monaco, 'Courier New', monospace;
    font-size: 0.85rem;
    resize: vertical;
    min-height: 200px;
  }
  .template-form textarea:focus {
    outline: none;
    border-color: #FF1493;
  }
  .template-form textarea::placeholder {
    color: #5f9ea0;
  }

  /* Form actions */
  .form-actions {
    margin-top: 1.5rem;
    display: flex;
    gap: 1rem;
    align-items: center;
  }

  /* Preview button */
  .preview-btn {
    display: inline-block;
    padding: 0.75rem 1.5rem;
    background: transparent;
    border: 1px solid #00CED1;
    color: #00CED1;
    font-size: 0.8rem;
    font-weight: bold;
    text-transform: uppercase;
  }
  .preview-btn:hover {
    background: #00CED1;
    color: #000000;
  }

  /* Send form */
  .send-form select {
    width: 100%;
    padding: 0.75rem;
    border: 1px solid #00CED1;
    background: #000000;
    color: #E0FFFF;
    font-family: inherit;
    font-size: 0.9rem;
  }
  .send-form select:focus {
    outline: none;
    border-color: #FF1493;
  }
  .radio-group {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }
  .radio-label {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    cursor: pointer;
    font-size: 0.9rem;
    text-transform: none;
    font-weight: normal;
  }
  .radio-label input[type='radio'] {
    width: auto;
    margin: 0;
  }
  .radio-label-inline {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    cursor: pointer;
    font-size: 0.9rem;
    text-transform: none;
    font-weight: normal;
    flex-shrink: 0;
  }
  .single-email-row {
    display: flex;
    flex-direction: row;
    align-items: center;
    gap: 0.75rem;
  }
  .single-email-input {
    flex: 1;
    padding: 0.5rem;
    font-size: 0.85rem;
  }
  .send-btn {
    background: #00CED1;
    font-size: 0.9rem;
    padding: 1rem 2rem;
  }
  .send-btn:hover {
    background: #20B2AA;
  }
  .send-result {
    margin-bottom: 1.5rem;
    padding: 1rem;
    border: 1px solid #00CED1;
    background: rgba(0, 206, 209, 0.1);
  }
  .result-summary {
    font-size: 1rem;
    font-weight: bold;
    margin: 0 0 0.5rem 0;
  }
  .result-errors {
    color: #ffc107;
    font-size: 0.8rem;
  }
  .result-errors ul {
    margin: 0.5rem 0;
    padding-left: 1.5rem;
  }

  /* History table */
  .history-table {
    width: 100%;
    border-collapse: collapse;
    margin-top: 1rem;
    border: 1px solid #00CED1;
  }
  .history-table th,
  .history-table td {
    padding: 0.75rem;
    text-align: left;
    border-bottom: 1px solid rgba(0, 206, 209, 0.3);
  }
  .history-table th {
    background: rgba(0, 206, 209, 0.2);
    color: #FF1493;
    font-size: 0.7rem;
    text-transform: uppercase;
    font-weight: bold;
  }
  .history-table tr:hover {
    background: rgba(0, 206, 209, 0.1);
  }
  .history-table .date {
    font-size: 0.8rem;
    color: #5f9ea0;
    white-space: nowrap;
  }
  .history-table .send-type {
    font-size: 0.75rem;
    font-weight: bold;
    text-transform: uppercase;
  }
  .history-table .send-type.blast {
    color: #FF1493;
  }
  .history-table .send-type.one-off {
    color: #00CED1;
  }
  /* Subscriber status styles */
  .status {
    font-size: 0.75rem;
    font-weight: bold;
    text-transform: uppercase;
  }
  .status.confirmed {
    color: #00CED1;
  }
  .status.pending {
    color: #FF1493;
  }
  .history-table .subject {
    color: #5f9ea0;
    font-size: 0.85rem;
    max-width: 200px;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .history-table .sent-count {
    color: #00CED1;
    font-weight: bold;
  }
  .history-table .failed-count {
    color: #5f9ea0;
  }
  .history-table .has-failures {
    color: #ff6b6b;
    font-weight: bold;
  }

  /* Table controls (search + count) */
  .history-controls,
  .table-controls {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 1rem;
    margin-bottom: 0.5rem;
  }
  .search-input {
    flex: 1;
    max-width: 300px;
  }

  /* Sortable headers */
  .sortable {
    cursor: pointer;
    user-select: none;
    position: relative;
    padding-right: 1.5rem !important;
  }
  .sortable:hover {
    background: rgba(255, 20, 147, 0.2);
  }
  .sort-indicator {
    position: absolute;
    right: 0.5rem;
    top: 50%;
    transform: translateY(-50%);
    font-size: 0.6rem;
  }
  .sort-indicator::after {
    content: '\\2195';
    opacity: 0.4;
  }
  .sortable.asc .sort-indicator::after {
    content: '\\25B2';
    opacity: 1;
  }
  .sortable.desc .sort-indicator::after {
    content: '\\25BC';
    opacity: 1;
  }

  /* Hidden row for search */
  .hidden-row {
    display: none;
  }

  /* Analytics page */
  .analytics-time-selector {
    display: flex;
    gap: 0.5rem;
    margin-bottom: 1.5rem;
  }
  .time-btn {
    padding: 0.5rem 1rem;
    font-size: 0.75rem;
    font-weight: bold;
    text-transform: uppercase;
    text-decoration: none;
    border: 1px solid #00CED1;
    color: #00CED1;
    background: transparent;
  }
  .time-btn:hover {
    background: rgba(0, 206, 209, 0.2);
    color: #00CED1;
  }
  .time-btn.active {
    background: #00CED1;
    color: #000000;
  }
  .analytics-chart-container {
    display: flex;
    gap: 1rem;
    margin-bottom: 2rem;
  }
  .analytics-y-axis {
    display: flex;
    flex-direction: column;
    justify-content: space-between;
    align-items: flex-end;
    padding-bottom: 24px;
    min-width: 40px;
  }
  .y-label {
    font-size: 0.65rem;
    color: #5f9ea0;
  }
  .analytics-chart {
    flex: 1;
    display: flex;
    align-items: flex-end;
    height: 200px;
    border-bottom: 1px solid #00CED1;
    border-left: 1px solid #00CED1;
    padding: 0 4px;
  }
  .chart-bar-wrapper {
    display: flex;
    flex-direction: column;
    align-items: center;
    height: 100%;
    justify-content: flex-end;
  }
  .chart-bar {
    width: 100%;
    background: linear-gradient(to top, #FF1493, #00CED1);
    min-height: 2px;
    transition: opacity 0.2s;
  }
  .chart-bar:hover {
    opacity: 0.8;
  }
  .chart-label {
    font-size: 0.6rem;
    color: #5f9ea0;
    margin-top: 4px;
    white-space: nowrap;
  }
  .analytics-empty {
    color: #5f9ea0;
    font-style: italic;
    padding: 2rem;
    text-align: center;
    border: 1px dashed #00CED1;
  }
  .analytics-summary {
    display: flex;
    gap: 2rem;
    padding: 1rem;
    border: 1px solid #00CED1;
    background: rgba(0, 206, 209, 0.05);
  }
  .summary-stat {
    display: flex;
    flex-direction: column;
    align-items: center;
  }
  .stat-value {
    font-size: 1.5rem;
    font-weight: bold;
    color: #FF1493;
  }
  .stat-label {
    font-size: 0.7rem;
    color: #5f9ea0;
    text-transform: uppercase;
  }
  "
}

fn history_scripts() -> String {
  "
  (function() {
    const table = document.getElementById('history-table');
    const search = document.getElementById('history-search');
    const countEl = document.getElementById('history-count');
    if (!table) return;

    const tbody = table.querySelector('tbody');
    const headers = table.querySelectorAll('th.sortable');
    let currentSort = { col: null, dir: null };

    // Sorting
    headers.forEach(th => {
      th.addEventListener('click', () => {
        const col = parseInt(th.dataset.col);
        const type = th.dataset.type;

        // Toggle direction
        let dir = 'asc';
        if (currentSort.col === col && currentSort.dir === 'asc') {
          dir = 'desc';
        }

        // Update UI
        headers.forEach(h => h.classList.remove('asc', 'desc'));
        th.classList.add(dir);
        currentSort = { col, dir };

        // Sort rows
        const rows = Array.from(tbody.querySelectorAll('tr'));
        rows.sort((a, b) => {
          let aVal = a.cells[col].textContent.trim();
          let bVal = b.cells[col].textContent.trim();

          if (type === 'number') {
            aVal = parseInt(aVal) || 0;
            bVal = parseInt(bVal) || 0;
          } else if (type === 'date') {
            aVal = new Date(aVal);
            bVal = new Date(bVal);
          }

          let cmp = 0;
          if (aVal < bVal) cmp = -1;
          else if (aVal > bVal) cmp = 1;

          return dir === 'asc' ? cmp : -cmp;
        });

        rows.forEach(row => tbody.appendChild(row));
      });
    });

    // Searching
    if (search) {
      search.addEventListener('input', () => {
        const query = search.value.toLowerCase();
        const rows = tbody.querySelectorAll('tr');
        let visibleCount = 0;

        rows.forEach(row => {
          const template = row.cells[2].textContent.toLowerCase();
          const subject = row.cells[3].textContent.toLowerCase();
          const matches = template.includes(query) || subject.includes(query);
          row.classList.toggle('hidden-row', !matches);
          if (matches) visibleCount++;
        });

        if (countEl) {
          countEl.textContent = 'Showing: ' + visibleCount + ' of ' + rows.length;
        }
      });
    }
  })();
  "
}
