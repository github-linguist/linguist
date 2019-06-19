# typed: strict

# Source: https://github.com/sorbet/sorbet-typed/blob/master/lib/ruby/all/open3.rbi

module Open3
  sig do
    params(
      cmd: T.any(String, T::Array[String]),
      opts: T::Hash[Symbol, T.untyped],
      block: T.proc.params(stdin: IO, stdout: IO, stderr: IO, wait_thr: Thread).void
    ).returns([IO, IO, IO, Thread])
  end
  def self.popen3(*cmd, **opts, &block); end

  sig do
    params(
      cmd: T.any(String, T::Array[String]),
      opts: T::Hash[Symbol, T.untyped],
      block: T.proc.params(stdin: IO, stdout: IO, wait_thr: Thread).void
    ).returns([IO, IO, Thread])
  end
  def self.popen2(*cmd, **opts, &block); end

  sig do
    params(
      cmd: T.any(String, T::Array[String]),
      opts: T::Hash[Symbol, T.untyped],
      block: T.proc.params(stdin: IO, stdout_and_stderr: IO, wait_thr: Thread).void
    ).returns([IO, IO, Thread])
  end
  def self.popen2e(*cmd, **opts, &block); end

  sig do
    params(
      cmd: T.any(String, T::Array[String]),
      stdin_data: T.nilable(String),
      binmode: T.any(FalseClass, TrueClass),
      opts: T::Hash[Symbol, T.untyped]
    ).returns([String, String, Process::Status])
  end
  def self.capture3(*cmd, stdin_data: '', binmode: false, **opts); end

  sig do
    params(
      cmd: T.any(String, T::Array[String]),
      stdin_data: T.nilable(String),
      binmode: T.any(FalseClass, TrueClass),
      opts: T::Hash[Symbol, T.untyped]
    ).returns([String, Process::Status])
  end
  def self.capture2(*cmd, stdin_data: nil, binmode: false, **opts); end

  sig do
    params(
      cmd: T.any(String, T::Array[String]),
      stdin_data: T.nilable(String),
      binmode: T.any(FalseClass, TrueClass),
      opts: T::Hash[Symbol, T.untyped]
    ).returns([String, Process::Status])
  end
  def self.capture2e(*cmd, stdin_data: nil, binmode: false, **opts); end

  sig do
    params(
      cmds: T.any(String, T::Array[String]),
      opts: T::Hash[Symbol, T.untyped],
      block: T.proc.params(first_stdin: IO, last_stdout: IO, wait_threads: T::Array[Thread]).void
    ).returns([IO, IO, T::Array[Thread]])
  end
  def self.pipeline_rw(*cmds, **opts, &block); end

  sig do
    params(
      cmds: T.any(String, T::Array[String]),
      opts: T::Hash[Symbol, T.untyped],
      block: T.proc.params(last_stdout: IO, wait_threads: T::Array[Thread]).void
    ).returns([IO, T::Array[Thread]])
  end
  def self.pipeline_r(*cmds, **opts, &block); end

  sig do
    params(
      cmds: T.any(String, T::Array[String]),
      opts: T::Hash[Symbol, T.untyped],
      block: T.proc.params(first_stdin: IO, wait_threads: T::Array[Thread]).void
    ).returns([IO, T::Array[Thread]])
  end
  def self.pipeline_w(*cmds, **opts, &block); end

  sig do
    params(
      cmds: T.any(String, T::Array[String]),
      opts: T::Hash[Symbol, T.untyped],
      block: T.proc.params(wait_threads: T::Array[Thread]).void
    ).returns(T::Array[Thread])
  end
  def self.pipeline_start(*cmds, **opts, &block); end

  sig do
    params(
      cmds: T.any(String, T::Array[String]),
      opts: T::Hash[Symbol, T.untyped]
    ).returns(T::Array[Process::Status])
  end
  def self.pipeline(*cmds, **opts); end
end