# https://hub.docker.com/_/microsoft-dotnet
FROM mcr.microsoft.com/dotnet/sdk:8.0 AS server-build
WORKDIR /Server

# Install dependencies
COPY *.fsproj .
RUN dotnet restore

# Copy everything
COPY . .

# Build and publish a release
RUN dotnet publish -c Release -o publish

## App Area

# Build runtime image
FROM mcr.microsoft.com/dotnet/aspnet:8.0
WORKDIR /App
COPY --from=server-build /Server/publish .
COPY content ./content
ENTRYPOINT ["dotnet", "writing.dll"]