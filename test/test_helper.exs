defmodule ETSHelpers do
  def safe_delete_table(table) do
    try do
      :ets.delete(table)
    rescue
      ArgumentError -> :ok
    catch
      :error, _ -> :ok
    end
  end
end

ExUnit.start()
