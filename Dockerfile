# https://hub.docker.com/_/microsoft-dotnet
FROM mcr.microsoft.com/dotnet/sdk:8.0 AS server-build
WORKDIR /Server

RUN apt update && apt install build-essential -y --no-install-recommends

# Install dependencies
COPY *.fsproj .
RUN dotnet restore

# Copy everything
COPY . .

# Build and publish a release
RUN make update_post_assets
RUN dotnet publish -c Release -o publish

## App Area

# Build runtime image
FROM mcr.microsoft.com/dotnet/aspnet:8.0
WORKDIR /App
COPY --from=server-build /Server/publish .
COPY content ./content
ENTRYPOINT ["dotnet", "writing.dll"]