require 'base64'
require 'net/smtp'
require 'tmail'
require 'mime/types'

class Email
  def initialize(from, to, subject, body, options={})
    @opts = {:attachments => [], :server => 'localhost'}.update(options)
    @msg = TMail::Mail.new
    @msg.from    = from
    @msg.to      = to
    @msg.subject = subject
    @msg.cc      = @opts[:cc]  if @opts[:cc]
    @msg.bcc     = @opts[:bcc] if @opts[:bcc]

    if @opts[:attachments].empty?
      # just specify the body
      @msg.body = body
    else
      # attach attachments, including the body
      @msg.body = "This is a multi-part message in MIME format.\n"

      msg_body = TMail::Mail.new
      msg_body.body = body
      msg_body.set_content_type("text","plain", {:charset => "ISO-8859-1"})
      @msg.parts << msg_body

      octet_stream = MIME::Types['application/octet-stream'].first

      @opts[:attachments].select {|file| File.readable?(file)}.each do |file|
        mime_type = MIME::Types.type_for(file).first || octet_stream
        @msg.parts << create_attachment(file, mime_type)
      end
    end
  end
  attr_reader :msg

  def create_attachment(file, mime_type)
    attach = TMail::Mail.new
    if mime_type.binary?
      attach.body = Base64.encode64(File.read(file))
      attach.transfer_encoding = 'base64'
    else
      attach.body = File.read(file)
    end
    attach.set_disposition("attachment", {:filename => file})
    attach.set_content_type(mime_type.media_type, mime_type.sub_type, {:name=>file})
    attach
  end

  # instance method to send an Email object
  def send
    args = @opts.values_at(:server, :port, :helo, :username, :password, :authtype)
    Net::SMTP.start(*args) do |smtp|
      smtp.send_message(@msg.to_s, @msg.from[0], @msg.to)
    end
  end

  # class method to construct an Email object and send it
  def self.send(*args)
    self.new(*args).send
  end
end

Email.send(
  'sender@sender.invalid',
  %w{ recip1@recipient.invalid recip2@example.com },
  'the subject',
  "the body\nhas lines",
  {
    :attachments => %w{ file1 file2 file3 },
    :server => 'mail.example.com',
    :helo => 'sender.invalid',
    :username => 'user',
    :password => 'secret'
  }
)
