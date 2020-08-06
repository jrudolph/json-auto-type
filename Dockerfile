ARG BASE_IMAGE_TAG
FROM hseeberger/scala-sbt:${BASE_IMAGE_TAG:-8u265_1.3.13_2.13.3} AS builder

RUN mkdir -p /tmp/project
WORKDIR /tmp/project

# COPY project/build.properties /tmp/project/project/
# prime sbt for cache
# FIXME: doesn't work currently because there are no scala sources in project
# RUN sbt compile

COPY build.sbt /tmp/project/
COPY project/* /tmp/project/project/

RUN ls /tmp/project/*
# warm up caches, if build doesn't change, we will use caches in the next run
RUN sbt update compile

COPY . /tmp/project/

RUN sbt "show web/assembly"

FROM adoptopenjdk/openjdk11:debian

COPY --from=builder /tmp/project/web/target/scala-2.13/*-assembly-*.jar /app.jar

EXPOSE 8080/tcp

CMD ["java", "-jar", "/app.jar"]