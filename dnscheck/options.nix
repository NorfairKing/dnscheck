{ lib }:
lib.types.submodule {
  options = {
    checks = lib.mkOption {
      description = "The checks to perform";
      type = lib.types.listOf (lib.types.oneOf [
        (lib.types.submodule {
          options = {
            domain = lib.mkOption {
              description = "domain";
              type = lib.types.str;
            };
            ip = lib.mkOption {
              default = null;
              description = "a addresses";
              type = lib.types.nullOr lib.types.str;
            };
            ips = lib.mkOption {
              default = null;
              description = "a addresses";
              type = lib.types.nullOr (lib.types.listOf lib.types.str);
            };
            type = lib.mkOption {
              type = lib.types.enum ["a"];
            };
          };
        })
        (lib.types.submodule {
          options = {
            domain = lib.mkOption {
              description = "domain";
              type = lib.types.str;
            };
            ip = lib.mkOption {
              default = null;
              description = "ipv6 addresses";
              type = lib.types.nullOr lib.types.str;
            };
            ips = lib.mkOption {
              default = null;
              description = "ipv6 addresses";
              type = lib.types.nullOr (lib.types.listOf lib.types.str);
            };
            type = lib.mkOption {
              type = lib.types.enum ["aaaa"];
            };
          };
        })
        (lib.types.submodule {
          options = {
            domain = lib.mkOption {
              description = "domain";
              type = lib.types.str;
            };
            type = lib.mkOption {
              type = lib.types.enum ["mx"];
            };
            value = lib.mkOption {
              default = null;
              description = "values: domain and priority";
              type = lib.types.nullOr lib.types.str;
            };
            values = lib.mkOption {
              default = null;
              description = "values: domain and priority";
              type = lib.types.nullOr (lib.types.listOf lib.types.str);
            };
          };
        })
        (lib.types.submodule {
          options = {
            domain = lib.mkOption {
              description = "domain";
              type = lib.types.str;
            };
            type = lib.mkOption {
              type = lib.types.enum ["txt"];
            };
            value = lib.mkOption {
              default = null;
              description = "text values";
              type = lib.types.nullOr lib.types.str;
            };
            values = lib.mkOption {
              default = null;
              description = "text values";
              type = lib.types.nullOr (lib.types.listOf lib.types.str);
            };
          };
        })
        (lib.types.submodule {
          options = {
            domain = lib.mkOption {
              description = "domain";
              type = lib.types.str;
            };
            type = lib.mkOption {
              type = lib.types.enum ["cname"];
            };
            value = lib.mkOption {
              default = null;
              description = "domains";
              type = lib.types.nullOr lib.types.str;
            };
            values = lib.mkOption {
              default = null;
              description = "domains";
              type = lib.types.nullOr (lib.types.listOf lib.types.str);
            };
          };
        })
        (lib.types.submodule {
          options = {
            domain = lib.mkOption {
              description = "domain";
              type = lib.types.str;
            };
            type = lib.mkOption {
              type = lib.types.enum ["ns"];
            };
            value = lib.mkOption {
              default = null;
              description = "domains";
              type = lib.types.nullOr lib.types.str;
            };
            values = lib.mkOption {
              default = null;
              description = "domains";
              type = lib.types.nullOr (lib.types.listOf lib.types.str);
            };
          };
        })
      ]);
    };
    retry-policy = lib.mkOption {
      default = {
        base-delay = null;
        max-retries = null;
      };
      description = "The retry policy for flaky checks due to network failures etc";
      type = lib.types.nullOr (lib.types.submodule {
        options = {
          base-delay = lib.mkOption {
            default = 100000;
            description = "The delay between the first and second try, in microseconds";
            type = lib.types.nullOr lib.types.ints.unsigned;
          };
          max-retries = lib.mkOption {
            default = 10;
            description = "The maximum number of retries";
            type = lib.types.nullOr lib.types.ints.unsigned;
          };
        };
      });
    };
  };
}
