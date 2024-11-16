ARG otp_vsn=27.1
FROM erlang:${otp_vsn}
LABEL org.label-schema.name='AMOC Arsenal' \
      org.label-schema.vendor='Erlang Solutions'

WORKDIR /amoc_arsenal

COPY ./ ./
RUN git clean -ffxd
RUN rebar3 release

ENV PATH "/amoc_arsenal/_build/default/rel/amoc_arsenal/bin:${PATH}"

CMD ["amoc_arsenal", "console", "-noshell", "-noinput", "+Bd"]
