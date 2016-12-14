# LruReg

## LRU Registry

- maintains a pool of process trees
- pool has a maximum size

### lru_reg_sup

start_link(registry_name, pool_size)
  if reg_server doesn't exist, start it
  otherwise, restart with updated pool size

### lru_reg_server

functions:
- add(registry_name, process_name) 
- delete(registry_name, process_name)
- touch(registry_name, process_name)
- present?(registry_name, process_name)
- absent?(registry_name, process_name)

state:
- process list
- pool size

### lru_reg compliant process

| Proc     | Func                            |
|----------|---------------------------------|
| sup      | proc_name(base)                 |
| sup      | term_to_otp(process_name, term) |
| sup      | otp_to_term(process_name)       |
| sup, wrk | touch(reg_name, proc_name)      |


