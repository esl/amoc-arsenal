ARG otp_vsn=25.3
FROM erlang:${otp_vsn}
MAINTAINER Erlang Solutions <mongoose-im@erlang-solutions.com>

WORKDIR /amoc_arsenal

COPY ./ ./
RUN git clean -ffxd
RUN rebar3 release

ENV PATH "/amoc_arsenal/_build/default/rel/amoc_arsenal/bin:${PATH}"

CMD ["amoc_arsenal", "console", "-noshell", "-noinput", "+Bd"]
