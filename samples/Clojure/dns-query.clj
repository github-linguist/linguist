(import java.net.InetAddress java.net.Inet4Address java.net.Inet6Address)

(doseq [addr (InetAddress/getAllByName "www.kame.net")]
  (cond
    (instance? Inet4Address addr) (println "IPv4:" (.getHostAddress addr))
    (instance? Inet6Address addr) (println "IPv6:" (.getHostAddress addr))))
