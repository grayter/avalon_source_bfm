library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library types;
use types.bus_data_types_pkg.all;

library bfm;

-- This package contains records and eneity necessary for requesting streaming
-- transactions. Users can send requests to the model which will then handle
-- the bit twiddling needed for realising that request. Requests will be
-- immediately ack'd and then worked on. Requests include information on
-- desired pause rates.

package avalon_source_bfm_pkg is
  type SOURCE_REQUEST_t is record
    transaction_id : std_ulogic_vector;
    packet_count   : positive;
    pause_pct      : natural range 0 to 99;
    valid          : std_ulogic;
  end record SOURCE_REQUEST_t;

  component avalon_source_bfm
    port (
      -- Clocking
      clk             : in  std_ulogic;
      reset           : in  std_ulogic;
      -- Requests
      stream_req      : in  SOURCE_REQUEST_t;
      stream_req_resp : out types.bus_data_types_pkg.ACK_t;
      -- Outputs
      packet          : out types.bus_data_types_pkg.AVALON_DATA_STREAM_PACKET_t;
      packet_ack      : in  types.bus_data_types_pkg.ACK_t
      );
  end component;
end avalon_source_bfm_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library types;
use types.bus_data_types_pkg.all;

library bfm;
use bfm.avalon_source_bfm_pkg.all;

library osvvm;
context osvvm.OsvvmContext;

entity avalon_source_bfm is
  port (
    -- Clocking
    clk             : in  std_ulogic;
    reset           : in  std_ulogic;
    -- Requests from testbenches etc
    stream_req      : in  SOURCE_REQUEST_t;
    stream_req_resp : out types.bus_data_types_pkg.ACK_t;
    -- Generated packet streams
    packet          : out types.bus_data_types_pkg.AVALON_DATA_STREAM_PACKET_t;
    packet_ack      : in  types.bus_data_types_pkg.ACK_t
    );
end avalon_source_bfm;

architecture bfm of avalon_source_bfm is
  signal buffered_request : SOURCE_REQUEST_t(transaction_id(stream_req.transaction_id'range));
  type bfm_state_t is (GET_REQUEST, GENERATE_PACKETS);

  signal bfm_state : bfm_state_t;
  signal hungry    : std_ulogic;
  signal packet_id : std_ulogic_vector(packet.data'range);
begin

  -- Ack handling, hungry etc
  stream_req_resp.ack <= '1' when (bfm_state = GET_REQUEST and stream_req.valid = '1') else '0';
  hungry              <= not packet.valid or packet_ack.ack;

  process(clk, reset) is
    variable RV         : RandomPType;
    variable gen_chance : integer;
  begin
    if rising_edge(clk) then
      packet.valid <= packet.valid and not packet_ack.ack;

      case bfm_state is
        when GET_REQUEST =>
          if stream_req.valid then
            buffered_request <= stream_req;
            bfm_state        <= GENERATE_PACKETS;
          end if;

        when GENERATE_PACKETS =>
          if hungry then
            gen_chance := RV.RandInt(0, 100);
            if gen_chance > buffered_request.pause_pct then
              packet.transaction_id <= buffered_request.transaction_id;

              packet.data <= packet_id;
              packet_id   <= std_ulogic_vector(unsigned(packet_id) + 1);

              packet.start <= '1' when (unsigned(packet_id) = 0)           else '0';
              packet.stop  <= '1' when (buffered_request.packet_count = 1) else '0';

              packet.valid <= '1';

              if buffered_request.packet_count = 1 then
                bfm_state <= GET_REQUEST;
                packet_id <= (others => '0');
              else
                buffered_request.packet_count <= buffered_request.packet_count - 1;
              end if;
            end if;
          end if;
      end case;
    end if;

    if reset then
      bfm_state    <= GET_REQUEST;
      packet.valid <= '0';
      RV.InitSeed(RV'instance_name);
      packet_id    <= (others => '0');
    end if;
  end process;

end architecture;
