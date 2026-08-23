# Prepare a list of data to be sent as multipart form data.
# @category Internet
# @param ~boundary Specify boundary to use for multipart/form-data.
# @param data data to insert
def http.multipart_form_data(~boundary=null(), data) =
  def default_boundary() =
    range = [...string.char.ascii.alphabet, ...string.char.ascii.number]
    l = list.init(12, fun (_) -> string.char.ascii.random(range))
    string.concat(l)
  end

  boundary = null.default(boundary, default_boundary)

  def mk_content(contents, entry) =
    data = entry.contents
    attributes = [("name", entry.name), ...entry.attributes]
    attributes =
      list.map(
        fun (v) -> "#{string(fst(v))}=#{string.quote(snd(v))}", attributes
      )

    attributes = string.concat(separator="; ", attributes)
    headers =
      list.map(fun (v) -> "#{string(fst(v))}: #{string(snd(v))}", entry.headers)

    headers = string.concat(separator="\r\n", headers)
    headers = headers == "" ? "" : "#{headers}\r\n"

    # This is for typing purposes
    (entry : unit)
    [
      ...contents,
      getter("--#{boundary}\r\n"),
      getter("Content-Disposition: form-data; #{attributes}\r\n"),
      getter(headers),
      getter("\r\n"),
      data,
      getter("\r\n")
    ]
  end

  contents = [...list.fold(mk_content, [], data), getter("--#{boundary}--\r\n")]
  contents = string.getter.concat(contents)
  contents =
    if
      list.for_all(fun (entry) -> getter.is_constant(entry.contents), data)
    then
      getter(string.getter.flush(contents))
    else
      contents
    end

  {contents=contents, boundary=boundary}
end

# Initiate a response handler with pre-filled values.
# @category Internet
# @method content_type Set `"Content-Type"` header
# @method data Set response data.
# @method headers Replace response headers.
# @method header Set a single header on the response
# @method json Set content-type to json and data to `json.stringify` of the argument
# @method redirect Set `status_code` and `Location:` header for a HTTP redirect response
# @method html Set content-type to html and data to argument value
# @method http_version Set http protocol version
# @method status_code Set response status code
# @method status_message Set response status message
def http.response(
  ~http_version="1.1",
  ~status_code=null(),
  ~status_message=null(),
  ~headers=[],
  ~content_type=null(),
  ~data=getter("")
) =
  status_code =
    status_code
      ??
        if
          http_version == "1.1"
        and
          headers["expect"] == "100-continue"
        and
          getter.get(data) == ""
        then
          100
        else
          200
        end

  http_version = ref(http_version)
  status_code = ref(status_code)
  status_message = ref(status_message)
  headers = ref(headers)
  content_type = ref(content_type)
  data = ref(data)
  status_sent = ref(false)
  headers_sent = ref(false)
  data_sent = ref(false)
  response_ended = ref(false)

  def mk_status() =
    status_sent := true
    http_version = http_version()
    status_code = status_code()
    status_code =
      if
        status_code == 100 and getter.get(data()) != ""
      then
        200
      else
        status_code
      end

    status_message = status_message() ?? http.codes[status_code]
    "HTTP/#{http_version} #{status_code} #{status_message}\r\n"
  end

  def mk_headers() =
    headers_sent := true
    headers = headers()
    content_type = content_type()
    data = data()
    headers =
      if
        getter.is_constant(data)
      then
        data = getter.get(data)
        if
          data != ""
        then
          ("Content-Length", "#{string.length(data)}")::headers
        else
          headers
        end
      else
        ("Transfer-Encoding", "chunked")::headers
      end

    headers =
      if
        null.defined(content_type) and null.get(content_type) != ""
      then
        ("Content-type", null.get(content_type))::headers
      else
        headers
      end

    headers = list.map(fun (v) -> "#{fst(v)}: #{snd(v)}", headers)
    headers = string.concat(separator="\r\n", headers)
    headers = if headers != "" then "#{headers}\r\n" else "" end
    "#{headers}\r\n"
  end

  def mk_data() =
    data_sent := true
    data = data()
    if
      getter.is_constant(data)
    then
      response_ended := true
      getter.get(data)
    else
      data = getter.get(data)
      response_ended := data == ""
      "#{string.hex_of_int(string.length(data))}\r\n#{data}\r\n"
    end
  end

  def response() =
    if
      response_ended()
    then
      ""
    elsif not status_sent() then mk_status()
    elsif not headers_sent() then mk_headers()
    else
      mk_data()
    end
  end

  def attr_method(sent, attr) =
    def set(v) =
      if
        sent()
      then
        error.raise(
          error.invalid, "HTTP response has already been sent for this value!"
        )
      end

      attr := v
    end

    def get() =
      attr()
    end

    set.{current=get}
  end

  def header(k, v) =
    headers := (k, v)::headers()
  end

  code = status_code

  def redirect(~status_code=301, location) =
    if
      status_sent()
    then
      error.raise(
        error.invalid, "HTTP response has already been sent for this value!"
      )
    end

    code := status_code
    header("Location", location)
  end

  def json(~compact=true, v) =
    if
      headers_sent()
    then
      error.raise(
        error.invalid, "HTTP response has already been sent for this value!"
      )
    end

    content_type := "application/json; charset=utf-8"
    data := json.stringify(v, compact=compact) ^ "\n"
  end

  def html(d) =
    if
      headers_sent()
    then
      error.raise(
        error.invalid, "HTTP response has already been sent for this value!"
      )
    end

    content_type := "text/html"
    data := d
  end

  def send_status(socket) =
    if not status_sent() then socket.write(mk_status()) end
  end

  def multipart_form(~boundary=null(), contents) =
    if
      headers_sent()
    then
      error.raise(
        error.invalid, "HTTP response has already been sent for this value!"
      )
    end

    form_data = http.multipart_form_data(boundary=boundary, contents)
    content_type := "multipart/form-data; boundary=#{form_data.boundary}"
    data := form_data.contents
  end

  response.{
    http_version=attr_method(status_sent, http_version),
    status_code=attr_method(status_sent, status_code),
    status_message=attr_method(status_sent, status_message),
    headers=attr_method(headers_sent, headers),
    header=header,
    redirect=redirect,
    json=json,
    html=html,
    content_type=attr_method(headers_sent, content_type),
    multipart_form=multipart_form,
    data=attr_method(data_sent, data),
    send_status=send_status,
    status_sent={status_sent()}
  }
end