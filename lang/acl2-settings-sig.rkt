(module acl2-settings-sig (lib "a-signature.rkt")
  (struct acl2-settings (acl2-loc admit-before-run?))
  acl2-settings->vector
  marshall-acl2-settings
  unmarshall-acl2-settings
  default-acl2-settings)