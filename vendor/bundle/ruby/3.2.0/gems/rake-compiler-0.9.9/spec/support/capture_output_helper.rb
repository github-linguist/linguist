module CaptureOutputHelper
  def capture_output(&block)
    old_stdout = $stdout
    old_stderr = $stderr

    stream_out = StringIO.new
    stream_err = StringIO.new

    begin
      $stdout = stream_out
      $stderr = stream_err
      yield
    ensure
      $stdout = old_stdout
      $stderr = old_stderr
    end
    stream_out.rewind
    stream_err.rewind

    [stream_out.read, stream_err.read]
  end
end
