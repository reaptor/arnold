# Build the static site, then serve it with Apache.
#
# Built from the repository root (`docker build -f site/Dockerfile .`), because the
# page imports the app's screenshots (docs/screenshots) and icon (src-tauri/app-icon.svg)
# rather than keeping copies. site/Dockerfile.dockerignore keeps the context to just those.
#
# The build stage pins itself to the builder's own architecture: its output is
# plain HTML and JS, so running Node under emulation to produce identical files
# would only cost time. Only the httpd stage follows the requested --platform.
FROM --platform=$BUILDPLATFORM node:22-alpine AS build
WORKDIR /app/site

COPY site/package.json site/package-lock.json ./
RUN npm ci

COPY docs/screenshots/ /app/docs/screenshots/
COPY src-tauri/app-icon.svg /app/src-tauri/app-icon.svg
COPY site/ ./
RUN npm run build

FROM httpd:2.4
COPY site/httpd-site.conf /usr/local/apache2/conf/extra/site.conf
RUN echo 'Include conf/extra/site.conf' >> /usr/local/apache2/conf/httpd.conf
COPY --from=build /app/site/dist/ /usr/local/apache2/htdocs/
