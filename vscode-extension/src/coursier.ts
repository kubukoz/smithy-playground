export type CoursierCall = {
  coursierArgs: string[];
  app: CoursierApp;
};

export type CoursierApp = {
  maven: {
    artifact: string;
    version: string;
  };
  args: string[];
};

export const buildArgs = (underlying: CoursierCall): string[] => [
  "launch",
  `${underlying.app.maven.artifact}:${underlying.app.maven.version}`,
  ...underlying.coursierArgs,
  "--",
  ...underlying.app.args,
];

export const withTracer =
  (enable: Boolean) =>
  (underlying: CoursierCall): CoursierCall =>
    enable
      ? {
          app: {
            maven: {
              artifact: "tech.neander:langoustine-tracer_3",
              version: "latest.release",
            },
            args: ["--", "cs", ...buildArgs(underlying)],
          },
          coursierArgs: ["--ttl", "0"],
        }
      : underlying;

export const withDebug =
  (enable: Boolean) =>
  (call: CoursierCall): CoursierCall =>
    enable
      ? {
          ...call,
          coursierArgs: [
            ...call.coursierArgs,
            "--java-opt",
            "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,quiet=y,address=5010",
          ],
        }
      : call;
