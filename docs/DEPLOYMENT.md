# WFP Data Quality App - Deployment Guide

This guide covers how to deploy the WFP Data Quality App using Docker for local development.

## Table of Contents

- [Docker Deployment](#docker-deployment)
- [Quick Start](#quick-start)
- [Building the Image](#building-the-image)
- [Running Locally](#running-locally)
- [Configuration](#configuration)
- [Troubleshooting](#troubleshooting)

## Docker Deployment

The application is containerized using Docker for consistent deployment across different environments. The Docker setup includes:

- **Base Image**: `rocker/shiny:4.4.3` (R + Shiny Server + Ubuntu)
- **Dependencies**: All required R packages and system libraries
- **Configuration**: Production-ready Shiny Server settings
- **Security**: Runs as non-root `shiny` user

## Quick Start

### Prerequisites
- [Docker](https://docs.docker.com/get-docker/) installed on your system
- 2GB+ available disk space

### Run the App
```bash
# Build and run in one command
docker build -t wfp-data-quality-app .
docker run -d -p 3838:3838 --name wfp-app wfp-data-quality-app

# Access the application
open http://localhost:3838
```

### Stop the App
```bash
docker stop wfp-app
docker rm wfp-app
```

## Building the Image

### Standard Build
```bash
docker build -t wfp-data-quality-app .
```

### Build Process Overview
The Docker build process includes:

1. **System Dependencies**: Install Ubuntu packages needed for R packages
2. **renv Setup**: Install renv for reproducible package management
3. **Package Restoration**: Use `renv::restore()` to install exact package versions from `renv.lock`
4. **Package Installation**: Install the WFP app as an R package using `devtools`
5. **App Setup**: Copy Shiny app files to the correct locations
6. **Configuration**: Apply custom Shiny Server settings
7. **Permissions**: Set proper ownership for the `shiny` user

**Build Time**: Approximately 3-5 minutes (first build), subsequent builds are faster due to Docker layer caching.

## Running Locally

### Basic Run
```bash
docker run -d -p 3838:3838 --name wfp-app wfp-data-quality-app
# Access at http://localhost:8080
```

### Container Management
```bash
# View running containers
docker ps

# View container logs
docker logs wfp-app

# Access container shell (for debugging)
docker exec -it wfp-app /bin/bash

# Stop and remove container
docker stop wfp-app && docker rm wfp-app
```

## Configuration

### Shiny Server Configuration
The app uses a custom `shiny-server.conf` file with production-optimized settings:

- **Port**: 3838 (standard Shiny port)
- **Upload Timeout**: 60 seconds
- **Idle Timeout**: 5 minutes
- **Logging**: Enabled with rotation
- **Security**: Runs as `shiny` user

### File Upload Limits
The app supports file uploads up to **500MB**. This is configured in both:
- Shiny app: `options(shiny.maxRequestSize = 500 * 1024^2)`
- Shiny Server: `app_init_timeout 60`

## Troubleshooting

### Common Issues

#### App Not Loading (HTTP 500 Error)
```bash
# Check container logs
docker logs wfp-app

# Common causes:
# - Missing R packages
# - File permission issues
# - Memory limits exceeded
```

### Log Locations

| Component | Log Location |
|-----------|--------------|
| Shiny Server | `/var/log/shiny-server/` |
| Container stdout | `docker logs wfp-app` |
| Access logs | `/var/log/shiny-server/access.log` |

## Security Considerations

- ✅ **Non-root user**: App runs as `shiny` user, not root
- ✅ **File permissions**: Proper ownership and permissions set
- ✅ **Dependency management**: Clean package installation and optimization

---

For technical questions about the architecture, see [ARCHITECTURE.md](ARCHITECTURE.md).