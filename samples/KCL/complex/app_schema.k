schema App:
    """The application model."""
    name: str
    replicas: int = 1
    labels?: {str:str} = {app = name}
    service?: Service
    containers?: {str: Container}

schema Service:
    """The service model."""
    $type?: str
    ports: [Port]

schema Port:
    """The port model."""
    port: int
    protocol: "TCP" | "UDP" | "SCTP" = "TCP"
    targetPort?: int | str

schema Container:
    """The container model."""
    image: str
    command?: [str]
    args?: [str]
    env?: [Env]
    volumes?: [Volume]
    resources?: Resource
    ports: [ContainerPort]

schema ContainerPort:
    """The container port model."""
    name?: str
    protocol: "TCP" | "UDP" | "SCTP" = "TCP"
    containerPort: int

    check:
        1 <= containerPort <= 65535, "containerPort must be between 1 and 65535, inclusive"

schema Env:
    name: str
    value: str

schema Volume:
    source: str
    path: str
    target: str
    readOnly?: bool = False

schema Resource:
    limits?: {str:}
    requests?: {str:}
