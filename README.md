# sistracia.com

Personal blog for sharing Sistracia's journey in programming.

## Development

### Prerequisite Tools

- [.NET](https://dotnet.microsoft.com/en-us/download) 8.0

### Install dependencies

```bash
dotnet restore
```

### Start Development

```bash
dotnet run
```

#### Using Docker

See the [Dockerfile here](./Dockerfile).

Create `.env` file, [see example](./web/.env.example).

```env
PORT=<ports that the server should listen on for requests>
HOSTNAME=<addresses that the server should listen on for requests>
ASPNETCORE_URLS=http://${HOSTNAME}:${PORT}
```

##### Run with Docker Compose

```bash
docker compose -f docker-compose.development.yaml up
```

## Deployment

### Using Docker

```bash
# Build
docker build -t sistracia .

# Tag
docker image tag sistracia:latest <docker-registry>/sistracia:latest

# Push
docker image push <docker-registry>/sistracia:latest
```

#### Try Run Docker Image

Create `.env` file, [see example](./web/.env.example).

```env
ASPNETCORE_URLS=<SERVER APP HOST AND PORT INSIDE Docker>
```

Run the Docker image

```bash
docker run --env-file ./.env -p <PUBLISHD PORT>:<SERVER APP PORT INSIDE Docker> sistracia
## or
docker run \
-e ASPNETCORE_URLS="<SERVER APP HOST AND PORT INSIDE Docker>" \
-p <PUBLISHD PORT>:<SERVER APP PORT INSIDE Docker> \
sistracia
```
