using System;

namespace QFoxFramework.BlazorAnalyzers.Demo.Data
{
    public class WeatherForecast
    {
        public required DateTime Date { get; init; }

        public required int TemperatureC { get; init; }

        public int TemperatureF => 32 + (int) (TemperatureC / 0.5556);

        public required string Summary { get; init; }
    }
}